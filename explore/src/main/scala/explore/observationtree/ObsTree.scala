// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ActionButtons
import explore.components.ToolbarTooltipOptions
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.model.AppContext
import explore.model.Focused
import explore.model.Group
import explore.model.GroupList
import explore.model.ObsIdSet
import explore.model.Observation
import explore.model.ObservationExecutionMap
import explore.model.ObservationList
import explore.model.enums.AppTab
import explore.syntax.ui.*
import explore.tabs.DeckShown
import explore.undo.UndoSetter
import explore.undo.Undoer
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ScienceBand
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.syntax.display.*
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.ConfirmDialog
import lucuma.react.primereact.DialogPosition
import lucuma.react.primereact.PrimeStyles
import lucuma.react.primereact.Tree
import lucuma.react.primereact.Tree.Node
import lucuma.typed.primereact.treeTreeMod.TreeNodeTemplateOptions
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.*
import monocle.Iso
import org.scalajs.dom
import org.scalajs.dom.Element
import queries.schemas.odb.ObsQueries

import scala.collection.immutable.SortedSet
import scala.scalajs.js

import ObsQueries.*

case class ObsTree(
  programId:             Program.Id,
  observations:          UndoSetter[ObservationList],
  groups:                UndoSetter[GroupList],
  groupsChildren:        Map[Option[Group.Id], List[Either[Observation, Group]]],
  obsExecutionTimes:     ObservationExecutionMap,
  undoer:                Undoer,
  focusedObs:            Option[Observation.Id], // obs explicitly selected for editing
  focusedTarget:         Option[Target.Id],
  focusedGroup:          Option[Group.Id],
  selectedObsIds:        List[Observation.Id],   // obs list selected in table
  setSummaryPanel:       Callback,
  expandedGroups:        View[Set[Group.Id]],
  deckShown:             View[DeckShown],
  copyCallback:          Callback,
  pasteCallback:         Callback,
  clipboardObsContents:  Option[ObsIdSet],
  allocatedScienceBands: SortedSet[ScienceBand],
  readonly:              Boolean
) extends ReactFnProps(ObsTree.component):
  private val selectedObsIdSet: Option[ObsIdSet] =
    focusedObs.map(ObsIdSet.one(_)).orElse(ObsIdSet.fromList(selectedObsIds))

  private val activeGroup: Option[Group.Id] =
    focusedGroup.orElse(focusedObs.flatMap(observations.get.get(_)).flatMap(_.groupId))

  private val copyDisabled: Boolean   = selectedObsIdSet.isEmpty
  private val pasteDisabled: Boolean  = clipboardObsContents.isEmpty
  private val deleteDisabled: Boolean = selectedObsIdSet.isEmpty && focusedGroup.isEmpty

  private def observationsText(observations: ObsIdSet): String =
    observations.idSet.size match
      case 1    => s"observation ${observations.idSet.head}"
      case more => s"$more observations"
  private def groupText(groupId: Group.Id): String             = s"group $groupId"

  private val copyText: Option[String]                               = selectedObsIdSet.map(observationsText)
  private def selectedText(obsIds: Iterable[Observation.Id]): String =
    obsIds.size match
      case 1    => s"observation ${obsIds.head}"
      case more => s"$more observations"
  private val pasteText: Option[String]                              =
    clipboardObsContents
      .map(_.idSet.toSortedSet)
      .map(selectedText)
      .map(_ + activeGroup.map(gid => s" into ${groupText(gid)}").orEmpty)
  private val deleteText: Option[String]                             =
    selectedObsIdSet.map(observationsText).orElse(focusedGroup.map(groupText))

  private def createNode(
    value:    Either[Observation, Group],
    isSystem: Boolean
  ): Node[Either[Observation, Group]] =
    Tree.Node(
      Tree.Id(value.fold(_.id.toString, _.id.toString)),
      value,
      draggable = !isSystem,
      droppable = !isSystem,
      children = value.toOption
        .map: group =>
          groupsChildren
            .get(group.id.some)
            .map(_.map(createNode(_, isSystem)))
            .orEmpty
        .orEmpty
    )

  // Root elements are already sorted by index
  val rootElements: List[Either[Observation, Group]] = groupsChildren.get(none).orEmpty

  val treeNodes: List[Node[Either[Observation, Group]]] =
    rootElements.filter(_.fold(_ => true, !_.system)).map(createNode(_, isSystem = false))

  val systemTreeNodes: List[Node[Either[Observation, Group]]] =
    rootElements.filter(_.fold(_ => false, _.system)).map(createNode(_, isSystem = true))

object ObsTree:
  private type Props = ObsTree

  /**
   * Iso to go between Group.Id and Tree.Id
   */
  private val groupTreeIdLens: Iso[Set[Group.Id], Set[Tree.Id]] =
    Iso[Set[Group.Id], Set[Tree.Id]](_.map(gId => Tree.Id(gId.toString)))(
      _.flatMap(v => Group.Id.parse(v.value))
    )

  private def scrollIfNeeded(targetObs: Observation.Id) =
    Callback {
      Option(dom.document.getElementById(s"obs-list-${targetObs.toString}"))
        .filterNot(js.isUndefined)
        .map { obsListElement =>
          val rect = obsListElement.getBoundingClientRect()
          if (rect.top < 0) obsListElement.scrollIntoView()
          if (rect.bottom > dom.window.innerHeight) obsListElement.scrollIntoView(false)
        }
    }

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      // .useState(none[NonNegInt])              // optIndex: Saved index into the observation list
      // .useEffectWithDepsBy((props, _, _) => (props.focusedObs, props.observations.get)): // set the index of the focused obs
      //   (props, ctx, optIndex) =>
      //     (focusedObs, obsList) =>
      //       focusedObs.fold(optIndex.setState(none)): obsId =>
      //         // there is a focused obsId, look for it in the list
      //         val foundIdx = obsList.getIndex(obsId)
      //         (optIndex.value, foundIdx) match
      //           case (_, Some(fidx)) =>
      //             optIndex.setState(fidx.some) // focused obs is in list
      //           case (None, None)       =>
      //             focusObs(props.programId, none, ctx) >> optIndex.setState(none)
      //           case (Some(oidx), None) =>
      //             // focused obs no longer exists, but we have a previous index.
      //             val newIdx = math.min(oidx.value, obsList.size.value - 1)
      //             obsList.toList
      //               .get(newIdx.toLong)
      //               .fold(
      //                 optIndex.setState(none) >> focusObs(props.programId, none, ctx)
      //               )(obsSumm =>
      //                 optIndex.setState(NonNegInt.from(newIdx).toOption) >>
      //                   focusObs(props.programId, obsSumm.id.some, ctx)
      //               )
      .useEffectWithDepsBy((props, _) => (props.focusedGroup, props.groups.get)): (props, ctx) =>
        (focusedGroup, groups) =>
          // If the focused group is not in the tree, reset the focused group
          focusedGroup
            .filter(groupId => !groups.contains(groupId))
            .as(focusGroup(props.programId, none, ctx))
            .getOrEmpty
      .useStateView(AddingObservation(false)) // adding new observation
      // Scroll to newly created/selected observation
      .useEffectWithDepsBy((props, _, _) => props.focusedObs): (_, _, _) =>
        focusedObs => focusedObs.map(scrollIfNeeded).getOrEmpty
      // Open the group (and all super-groups) of the focused observation
      // .useEffectWithDepsBy((props, _, _) => (props.focusedObs, props.groups.get)): (props, _, _) =>
      //   case (None, _)             => Callback.empty
      //   case (Some(obsId), groups) =>
      //     val groupsToAddFocus = groups
      //       .parentKeys(obsId.asLeft)
      //       .flatMap(_.toOption)

      //     props.expandedGroups.mod(_ ++ groupsToAddFocus).when_(groupsToAddFocus.nonEmpty)
      .render: (props, ctx, adding) =>
        import ctx.given

        val expandedGroups: View[Set[Tree.Id]] = props.expandedGroups.as(groupTreeIdLens)

        def onDragDrop(e: Tree.DragDropEvent[Either[Observation, Group]]): Callback =
          if (e.dropNode.exists(node => node.data.isLeft))
            Callback.empty
          else {
            val dragNode: Either[Observation, Group] = e.dragNode.data
            val dropNodeId: Option[Group.Id]         = e.dropNode.flatMap(_.data.toOption.map(_.id))

            val dropIndex: NonNegShort =
              NonNegShort.from(e.dropIndex.toShort).getOrElse(NonNegShort.unsafeFrom(0))

            val newParentGroupIndex: NonNegShort =
              if dropIndex.value == 0 then dropIndex
              else
                props.groupsChildren
                  .get(dropNodeId)
                  .flatMap(_.lift(dropIndex.value.toInt))
                  .map(_.fold(_.groupIndex, _.parentIndex))
                  .getOrElse(NonNegShort.unsafeFrom(Short.MaxValue))

            dragNode
              .fold(
                obs =>
                  ObsActions
                    .obsGroupInfo(obs.id)
                    .set(props.observations)((dropNodeId, newParentGroupIndex).some),
                group =>
                  ObsActions
                    .groupParentInfo(group.id)
                    .set(props.groups)((dropNodeId, newParentGroupIndex).some)
              ) >>
              // Open the group we moved to
              dropNodeId.map(id => props.expandedGroups.mod(_ + id)).getOrEmpty
          }

        val deleteObsList: List[Observation.Id] => Callback =
          selectedObsIds =>
            ConfirmDialog.confirmDialog(
              message = <.div(s"This action will delete ${props.selectedText(selectedObsIds)}."),
              header = "Observations delete",
              acceptLabel = "Yes, delete",
              position = DialogPosition.Top,
              accept = ObsActions
                .obsExistence(selectedObsIds, postMessage = ToastCtx[IO].showToast(_))
                .mod(props.observations)(_ => selectedObsIds.map(_ => none)),
              acceptClass = PrimeStyles.ButtonSmall,
              rejectClass = PrimeStyles.ButtonSmall,
              icon = Icons.SkullCrossBones(^.color.red)
            )

        val deleteGroup: Group.Id => Callback = groupId =>
          ObsActions
            .groupExistence(
              groupId,
              gid => focusGroup(props.programId, gid.some, ctx)
            )
            .set(props.groups)(none)
            .showToastCB(s"Deleted group ${groupId.shortName}")

        def renderItem(
          nodeValue: Either[Observation, Group],
          options:   TreeNodeTemplateOptions
        ): VdomNode =
          nodeValue match
            case Left(obs)    =>
              val selected: Boolean = props.focusedObs.contains_(obs.id)

              <.a(
                ^.id        := s"obs-list-${obs.id.toString}",
                ^.href      := ctx.pageUrl(
                  AppTab.Observations,
                  props.programId,
                  Focused.singleObs(obs.id, props.focusedTarget)
                ),
                // Disable link dragging to enable tree node dragging
                ^.draggable := false,
                ExploreStyles.ObsItem |+| ExploreStyles.SelectedObsItem.when_(selected),
                ^.onClick ==> linkOverride(
                  focusObs(props.programId, obs.id.some, ctx)
                )
              )(
                ObsBadge(
                  obs,
                  props.obsExecutionTimes.getPot(obs.id).map(_.programTimeEstimate),
                  ObsBadge.Layout.ObservationsTab,
                  selected = selected,
                  setStateCB = ObsActions
                    .obsEditState(obs.id)
                    .set(props.observations)
                    .compose((_: ObservationWorkflowState).some)
                    .some,
                  setSubtitleCB = ObsActions
                    .obsEditSubtitle(obs.id)
                    .set(props.observations)
                    .compose((_: Option[NonEmptyString]).some)
                    .some,
                  deleteCB = deleteObsList(List(obs.id)),
                  cloneCB = cloneObs(
                    props.programId,
                    List(obs.id),
                    obs.groupId, // Clone to the same group
                    props.observations,
                    ctx
                  ).switching(adding.async, AddingObservation(_))
                    .withToast(s"Duplicating obs ${obs.id}")
                    .runAsync
                    .some,
                  setScienceBandCB = (
                    (b: ScienceBand) =>
                      ObsActions.obsScienceBand(obs.id).set(props.observations)(b.some)
                  ).some,
                  allocatedScienceBands = props.allocatedScienceBands,
                  readonly = props.readonly
                )
              )
            case Right(group) =>
              val isEmpty: Boolean =
                props.groupsChildren.get(group.id.some).forall(_.isEmpty)

              GroupBadge(
                group,
                selected = props.focusedGroup.contains_(group.id),
                onClickCB = linkOverride(
                  focusGroup(props.programId, group.id.some, ctx)
                ),
                href = ctx.pageUrl(
                  AppTab.Observations,
                  props.programId,
                  Focused.group(group.id)
                ),
                deleteCB = deleteGroup(group.id),
                isEmpty = isEmpty,
                readonly = props.readonly || group.system
              )

        val expandFocusedGroup: Callback = props.expandedGroups.mod(_ ++ props.focusedGroup)

        val isSystemGroupFocused: Boolean =
          props.activeGroup
            .flatMap(props.groups.get.get(_))
            .exists(_.system)

        val tree: VdomNode =
          if (props.deckShown.get === DeckShown.Shown) {
            React.Fragment(
              <.div(ExploreStyles.TreeToolbar)(
                React
                  .Fragment(
                    <.span(
                      Button(
                        severity = Button.Severity.Success,
                        icon = Icons.New,
                        label = "Obs",
                        disabled = adding.get.value || isSystemGroupFocused,
                        loading = adding.get.value,
                        tooltip = "Add a new Observation",
                        tooltipOptions = ToolbarTooltipOptions.Default,
                        onClick = insertObs(
                          props.programId,
                          props.activeGroup, // Set the active group as the new obs parent if it is selected
                          props.observations,
                          adding,
                          ctx
                        ).runAsync *> expandFocusedGroup
                      ).mini.compact,
                      Button(
                        severity = Button.Severity.Success,
                        icon = Icons.New,
                        label = "Group",
                        disabled = adding.get.value || isSystemGroupFocused,
                        loading = adding.get.value,
                        tooltip = "Add a new Group",
                        tooltipOptions = ToolbarTooltipOptions.Default,
                        onClick = insertGroup(
                          props.programId,
                          // Set the focused group as the new group parent if it is selected
                          props.focusedGroup,
                          props.groups,
                          adding,
                          ctx
                        ).runAsync *> expandFocusedGroup
                      ).mini.compact
                    ),
                    UndoButtons(props.undoer, size = PlSize.Mini, disabled = adding.get.value),
                    ActionButtons(
                      ActionButtons.ButtonProps(
                        props.copyCallback,
                        disabled = props.copyDisabled,
                        tooltipExtra = props.copyText
                      ),
                      ActionButtons.ButtonProps(
                        props.pasteCallback,
                        disabled = props.pasteDisabled,
                        tooltipExtra = props.pasteText
                      ),
                      ActionButtons.ButtonProps(
                        props.selectedObsIdSet
                          .map(obsIdSet => deleteObsList(obsIdSet.idSet.toList))
                          .orElse(props.focusedGroup.map(deleteGroup))
                          .orEmpty,
                        disabled = props.deleteDisabled,
                        tooltipExtra = props.deleteText
                      )
                    )
                  )
                  .unless(props.readonly),
                Button(
                  severity = Button.Severity.Secondary,
                  outlined = true,
                  disabled = false,
                  tooltip = "Hide Observation Tree",
                  tooltipOptions = ToolbarTooltipOptions.Default,
                  icon = Icons.ArrowLeftFromLine,
                  clazz = ExploreStyles.ObsTreeHideShow,
                  onClick = props.deckShown.mod(_.flip)
                ).mini.compact
              ),
              <.div(
                Button(
                  severity = Button.Severity.Secondary,
                  icon = Icons.ListIcon,
                  label = "Observations Summary",
                  onClick = focusObs(props.programId, none, ctx) >> props.setSummaryPanel,
                  clazz = ExploreStyles.ButtonSummary
                )
              ),
              <.div(^.overflow := "auto")(
                Tree(
                  props.treeNodes,
                  renderItem,
                  expandedKeys = expandedGroups.get,
                  onToggle = expandedGroups.set,
                  dragDropScope = if (props.readonly) js.undefined else "obs-tree",
                  onDragDrop = if (props.readonly) js.undefined else onDragDrop
                ),
                Tree(
                  props.systemTreeNodes,
                  renderItem,
                  expandedKeys = expandedGroups.get,
                  onToggle = expandedGroups.set
                )
              )
            )
          } else EmptyVdom

        <.div(ExploreStyles.ObsTreeWrapper)(tree)
