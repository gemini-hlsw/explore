// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.data.NonEmptySet
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
import explore.model.enums.GroupWarning
import explore.syntax.ui.*
import explore.tabs.DeckShown
import explore.undo.UndoSetter
import explore.undo.Undoer
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.hooks.Hooks.UseRef
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
  parentGroups:          Either[Observation.Id, Group.Id] => List[Group.Id],
  groupWarnings:         Map[Group.Id, NonEmptySet[GroupWarning]],
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
  addingObservation:     View[AddingObservation],
  readonly:              Boolean
) extends ReactFnProps(ObsTree.component):
  private val selectedObsIdSet: Option[ObsIdSet] =
    focusedObs.map(ObsIdSet.one(_)).orElse(ObsIdSet.fromList(selectedObsIds))

  private val activeGroup: Option[Group.Id] =
    focusedGroup.orElse(focusedObs.flatMap(observations.get.get(_)).flatMap(_.groupId))

  private val focusedObsOrGroup: Option[Either[Observation.Id, Group.Id]] =
    focusedObs.map(_.asLeft).orElse(focusedGroup.map(_.asRight))

  private def groupInfo(
    elem: Either[Observation.Id, Group.Id]
  ): Option[(Option[Group.Id], NonNegShort)] =
    elem.fold(
      observations.get.get(_).map(obs => (obs.groupId, obs.groupIndex)),
      groups.get.get(_).map(group => (group.parentId, group.parentIndex))
    )

  private val focusedGroupInfo: Option[(Option[Group.Id], NonNegShort)] =
    focusedObsOrGroup.flatMap(groupInfo)

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
  private val rootElements: List[Either[Observation, Group]] = groupsChildren.get(none).orEmpty

  private val treeNodes: List[Node[Either[Observation, Group]]] =
    rootElements.filter(_.fold(_ => true, !_.system)).map(createNode(_, isSystem = false))

  private val systemTreeNodes: List[Node[Either[Observation, Group]]] =
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
    ScalaFnComponent[Props]: props =>
      // refocus if current focus ceases to exist
      inline def refocus(
        prevGroupInfo: UseRef[Option[(Option[Group.Id], NonNegShort)]],
        ctx:           AppContext[IO]
      ): HookResult[Unit] =
        useEffectWithDeps((props.focusedObsOrGroup, props.observations.get, props.groups.get)):
          (focusedObsOrGroup, obsList, groupList) =>
            focusedObsOrGroup.fold(prevGroupInfo.set(none)): elemId =>
              (prevGroupInfo.value, elemId.fold(obsList.contains(_), groupList.contains(_))) match
                case (_, true)           =>
                  prevGroupInfo.set(props.focusedGroupInfo)
                case (Some(prev), false) => // Previously focused element no longer exists
                  val prevGroup: Option[Group.Id]                    = prev._1
                  val prevIndex: NonNegShort                         = prev._2
                  val newElement: Option[Either[Observation, Group]] =
                    props.groupsChildren
                      .get(prevGroup)
                      .flatMap:
                        _.get(math.max(0, prevIndex.value - 1))
                      .orElse:
                        prevGroup.flatMap(groupList.get(_).map(_.asRight))

                  prevGroupInfo.set:
                    newElement.flatMap(e => props.groupInfo(e.bimap(_.id, _.id)))
                  >>
                    newElement
                      .fold(focusObs(props.programId, none, ctx)):
                        _.fold(
                          obs => focusObs(props.programId, obs.id.some, ctx),
                          group => focusGroup(props.programId, group.id.some, ctx)
                        )
                case _                   => Callback.empty

      for
        ctx           <- useContext(AppContext.ctx)
        // Saved index into the observation tree
        prevGroupInfo <- useRef(props.focusedGroupInfo)
        // refocus if current focus ceases to exist
        _             <- refocus(prevGroupInfo, ctx)
        adding        <- useStateView(AddingObservation(false))
        // Scroll to newly created/selected observation
        _             <- useEffectWithDeps(props.focusedObs): focusedObs =>
                           focusedObs.map(scrollIfNeeded).getOrEmpty
        // Open the group (and all super-groups) of the focused observation
        _             <- useEffectWithDeps(props.activeGroup):
                           _.map: activeGroupId =>
                             props.expandedGroups.mod:
                               _ ++ props.parentGroups(activeGroupId.asRight) + activeGroupId
                           .orEmpty
      yield
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

            val dropNodeChildren: Option[List[Either[Observation, Group]]] =
              props.groupsChildren
                .get(dropNodeId)
                .map(_.filter(_ =!= dragNode))

            val newParentGroupIndex: NonNegShort =
              if dropIndex.value == 0 then dropIndex
              else
                dropNodeChildren
                  .flatMap(_.lift(dropIndex.value.toInt - 1))
                  .map(_.fold(_.groupIndex, _.parentIndex) |+| 1)
                  .getOrElse(NonNegShort.unsafeFrom(0))

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
              ) >> // Open the group we moved to
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
                  (AppTab.Observations,
                   props.programId,
                   Focused.singleObs(obs.id, props.focusedTarget)
                  ).some
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
                    .withToastDuring(s"Duplicating obs ${obs.id}")
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
                props.groupWarnings.get(group.id),
                selected = props.focusedGroup.contains_(group.id),
                onClickCB = linkOverride:
                  focusGroup(props.programId, group.id.some, ctx)
                ,
                href = ctx.pageUrl(
                  (
                    AppTab.Observations,
                    props.programId,
                    Focused.group(group.id)
                  ).some
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
