// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.GroupQueries
import explore.components.ActionButtons
import explore.components.ToolbarTooltipOptions
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.data.tree.KeyedIndexedTree
import explore.data.tree.Node as ExploreNode
import explore.data.tree.Tree as ExploreTree
import explore.model.AppContext
import explore.model.Focused
import explore.model.Group
import explore.model.GroupTree
import explore.model.GroupTree.syntax.*
import explore.model.ObsIdSet
import explore.model.Observation
import explore.model.ObservationExecutionMap
import explore.model.ServerIndexed
import explore.model.enums.AppTab
import explore.model.reusability.given
import explore.tabs.DeckShown
import explore.undo.UndoSetter
import explore.undo.Undoer
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsStatus
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.syntax.display.*
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
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

import scala.scalajs.js

import ObsQueries.*

case class ObsList(
  observations:         UndoSetter[ObservationList],
  obsExecutionTimes:    ObservationExecutionMap,
  undoer:               Undoer,
  programId:            Program.Id,
  focusedObs:           Option[Observation.Id],
  focusedTarget:        Option[Target.Id],
  focusedGroup:         Option[Group.Id],
  setSummaryPanel:      Callback,
  groups:               UndoSetter[GroupTree],
  expandedGroups:       View[Set[Group.Id]],
  deckShown:            View[DeckShown],
  copyCallback:         Callback,
  pasteCallback:        Callback,
  clipboardObsContents: Option[ObsIdSet],
  readonly:             Boolean
) extends ReactFnProps(ObsList.component):
  private val activeGroup: Option[Group.Id] = focusedGroup.orElse:
    focusedObs.flatMap(groups.get.obsGroupId)

  private val copyDisabled: Boolean   = focusedObs.isEmpty
  private val pasteDisabled: Boolean  = clipboardObsContents.isEmpty
  private val deleteDisabled: Boolean = focusedObs.isEmpty && focusedGroup.isEmpty

  private def observationText(obsId: Observation.Id): String = s"observation $obsId"
  private def groupText(groupId:     Group.Id): String       = s"group $groupId"

  private val copyText: Option[String]     = focusedObs.map(observationText)
  private val selectedText: Option[String] =
    clipboardObsContents.map: obdIdSet =>
      obdIdSet.idSet.size match
        case 1    => s"observation ${obdIdSet.idSet.head}"
        case more => s"$more observations"
  private val pasteText: Option[String]    =
    selectedText.map(_ + activeGroup.map(gid => s" into ${groupText(gid)}").orEmpty)
  private val deleteText: Option[String]   =
    focusedObs.map(observationText).orElse(focusedGroup.map(groupText))

object ObsList:
  private type Props = ObsList

  /**
   * Iso to go between Group.Id and Tree.Id
   */
  private val groupTreeIdLens: Iso[Set[Group.Id], Set[Tree.Id]] =
    Iso[Set[Group.Id], Set[Tree.Id]](_.map(gId => Tree.Id(gId.toString)))(
      _.flatMap(v => Group.Id.parse(v.value))
    )

  /**
   * Iso between GroupTree and primereact.Tree nodes
   */
  private def groupTreeNodeIsoBuilder(
    allObservations: ObservationList
  ): Iso[GroupTree, List[Node[GroupTree.Value]]] =
    Iso[GroupTree, List[Node[GroupTree.Value]]](groupTree =>
      def createNode(node: GroupTree.Node): Node[GroupTree.Value] =
        val isSystemGroup: Boolean    = node.value.elem.toOption.exists(_.system)
        val isCalibrationObs: Boolean = node.value.elem.left.toOption
          .flatMap(obsId => allObservations.getValue(obsId))
          .exists(_.isCalibration)

        Tree.Node(
          Tree.Id(node.value.id.fold(_.toString, _.toString)),
          node.value,
          draggable = !isCalibrationObs && !isSystemGroup,
          // Setting `droppable` to false for groups will break the tree.
          // We handle the case in `onDragDrop`. See https://github.com/primefaces/primereact/issues/6976.
          droppable = !isCalibrationObs,
          clazz = ExploreStyles.UndroppableNode.when_(isSystemGroup),
          children = node.children.map(createNode)
        )

      groupTree.toTree.children.map(createNode)
    )(nodes =>
      def nodeToTree(node: Node[GroupTree.Value]): GroupTree.Node =
        ExploreNode(node.data, children = node.children.map(nodeToTree).toList)

      KeyedIndexedTree.fromTree(ExploreTree(nodes.map(nodeToTree)), _.id)
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
      // Saved index into the observation list
      .useState(none[NonNegInt])
      .useEffectWithDepsBy((props, _, _) => (props.focusedObs, props.observations.get)):
        (props, ctx, optIndex) =>
          params =>
            val (focusedObs, obsList) = params

            focusedObs.fold(optIndex.setState(none)): obsId =>
              // there is a focused obsId, look for it in the list
              val foundIdx = obsList.getIndex(obsId)
              (optIndex.value, foundIdx) match
                case (_, Some(fidx))    =>
                  optIndex.setState(fidx.some) // focused obs is in list
                case (None, None)       =>
                  setObs(props.programId, none, ctx) >> optIndex.setState(none)
                case (Some(oidx), None) =>
                  // focused obs no longer exists, but we have a previous index.
                  val newIdx = math.min(oidx.value, obsList.length.value - 1)
                  obsList.toList
                    .get(newIdx.toLong)
                    .fold(
                      optIndex.setState(none) >> setObs(props.programId, none, ctx)
                    )(obsSumm =>
                      optIndex.setState(NonNegInt.from(newIdx).toOption) >>
                        setObs(props.programId, obsSumm.id.some, ctx)
                    )
      .useEffectWithDepsBy((props, _, _) => (props.focusedGroup, props.groups.get)):
        (props, ctx, _) =>
          (focusedGroup, groups) =>
            // If the focused group is not in the tree, reset the focused group
            focusedGroup
              .filter(g => !groups.contains(g.asRight))
              .as(setGroup(props.programId, none, ctx))
              .getOrEmpty
      // adding new observation
      .useStateView(AddingObservation(false))
      // treeNodes
      .useMemoBy((props, _, _, _) => (props.groups.model.reuseByValue, props.observations.get)):
        (_, _, _, _) => (groups, observations) => groups.as(groupTreeNodeIsoBuilder(observations))
      // Scroll to newly created/selected observation
      .useEffectWithDepsBy((props, _, _, _, _) => props.focusedObs): (_, _, _, _, _) =>
        focusedObs => focusedObs.map(scrollIfNeeded).getOrEmpty
      // Open the group (and all super-groups) of the focused observation
      .useEffectWithDepsBy((props, _, _, _, _) => (props.focusedObs, props.groups.get)):
        (props, _, _, _, _) =>
          case (None, _)             => Callback.empty
          case (Some(obsId), groups) =>
            val groupsToAddFocus = groups
              .parentKeys(obsId.asLeft)
              .flatMap(_.toOption)

            props.expandedGroups.mod(_ ++ groupsToAddFocus).when_(groupsToAddFocus.nonEmpty)
      .render: (props, ctx, _, adding, treeNodes) =>
        import ctx.given

        val expandedGroups: View[Set[Tree.Id]] = props.expandedGroups.as(groupTreeIdLens)

        def onDragDrop(e: Tree.DragDropEvent[GroupTree.Value]): Callback =
          // If PrimeReact fixes https://github.com/primefaces/primereact/issues/6976,
          // then we can remove the 2nd check and set `droppable` to false for groups.
          if (
            e.dropNode
              .exists(node => node.data.elem.isLeft || node.data.elem.toOption.exists(_.system))
          )
            Callback.empty
          else {
            val dragNode: ServerIndexed[Either[Observation.Id, Group]] = e.dragNode.data
            val dropNodeId: Option[Group.Id]                           = e.dropNode.flatMap(_.data.id.toOption)

            val dropIndex: NonNegShort =
              NonNegShort.from(e.dropIndex.toShort).getOrElse(NonNegShort.unsafeFrom(0))

            val newParentGroupIndex: NonNegShort =
              if dropIndex.value == 0 then dropIndex
              else
                val groupTree: GroupTree = props.groups.get

                val newSiblings: List[GroupTree.Node] = dropNodeId
                  .flatMap: groupId =>
                    groupTree.getNodeAndIndexByKey(groupId.asRight)
                  .map(_._1.children)
                  .getOrElse(groupTree.rootChildren)

                // Get the parentIndex of the previous node and add one.
                newSiblings
                  .get(dropIndex.value.toInt - 1)
                  .map: previousNode =>
                    NonNegShort.unsafeFrom((previousNode.value.parentIndex.value + 1).toShort)
                  .getOrElse(NonNegShort.unsafeFrom(0))

            dragNode.id
              .fold(
                obsId =>
                  ObsQueries.moveObservation[IO](obsId, dropNodeId, newParentGroupIndex.some),
                groupId => GroupQueries.moveGroup[IO](groupId, dropNodeId, newParentGroupIndex.some)
              )
              .runAsync >>
              treeNodes.value.set(e.value.toList) >>
              // Open the group we moved to
              dropNodeId.map(id => props.expandedGroups.mod(_ + id)).getOrEmpty
          }

        val deleteObs: Observation.Id => Callback = oid =>
          obsExistence(
            oid,
            o => setObs(props.programId, o.some, ctx)
          )
            .mod(props.observations)(obsListMod.delete)
            .showToastCB(s"Deleted obs ${oid.shortName}")

        val deleteGroup: Group.Id => Callback = gid =>
          groupExistence(
            gid,
            g => setGroup(props.programId, g.some, ctx)
          )
            .mod(props.groups)(groupTreeMod.delete)
            .showToastCB(s"Deleted group ${gid.shortName}")

        def renderItem(nodeValue: GroupTree.Value, options: TreeNodeTemplateOptions): VdomNode =
          nodeValue.elem match
            case Left(obsId)  =>
              props.observations.get
                .getValue(obsId)
                .map: obs =>
                  val selected: Boolean = props.focusedObs.contains_(obsId)

                  <.a(
                    ^.id        := s"obs-list-${obsId.toString}",
                    ^.href      := ctx.pageUrl(
                      AppTab.Observations,
                      props.programId,
                      Focused.singleObs(obsId, props.focusedTarget)
                    ),
                    // Disable link dragging to enable tree node dragging
                    ^.draggable := false,
                    ExploreStyles.ObsItem |+| ExploreStyles.SelectedObsItem.when_(selected),
                    ^.onClick ==> linkOverride(
                      setObs(props.programId, obsId.some, ctx)
                    )
                  )(
                    ObsBadge(
                      obs,
                      props.obsExecutionTimes.getPot(obsId).map(_.programTimeEstimate),
                      ObsBadge.Layout.ObservationsTab,
                      selected = selected,
                      setStatusCB = obsEditStatus(obsId)
                        .set(props.observations)
                        .compose((_: ObsStatus).some)
                        .some,
                      setActiveStatusCB = obsActiveStatus(obsId)
                        .set(props.observations)
                        .compose((_: ObsActiveStatus).some)
                        .some,
                      setSubtitleCB = obsEditSubtitle(obsId)
                        .set(props.observations)
                        .compose((_: Option[NonEmptyString]).some)
                        .some,
                      deleteCB = deleteObs(obsId),
                      cloneCB = cloneObs(
                        props.programId,
                        obsId,
                        props.groups.get.obsGroupId(obsId), // Clone to the same group
                        props.observations,
                        ctx,
                        adding.async.set(AddingObservation(true)),
                        adding.async.set(AddingObservation(false))
                      )
                        .withToast(s"Duplicating obs ${id}")
                        .runAsync
                        .some,
                      readonly = props.readonly
                    )
                  )
            case Right(group) =>
              val isEmpty = props.groups.get
                .getNodeAndIndexByKey(group.id.asRight)
                .exists(_._1.children.isEmpty)
              GroupBadge(
                group,
                selected = props.focusedGroup.contains_(group.id),
                onClickCB = linkOverride(
                  setGroup(props.programId, group.id.some, ctx)
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
          props.focusedGroup
            .flatMap: groupId =>
              props.groups.get.getNodeAndIndexByKey(groupId.asRight)
            .exists(_._1.value.elem.toOption.exists(_.system))

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
                          // Set the focused group as the new obs parent if it is selected
                          props.focusedGroup,
                          props.observations.get.length,
                          props.observations,
                          props.groups.model,
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
                        props.focusedObs
                          .map(deleteObs)
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
                  onClick = setObs(props.programId, none, ctx) >> props.setSummaryPanel,
                  clazz = ExploreStyles.ButtonSummary
                )
              ),
              <.div(^.overflow := "auto")(
                Tree(
                  treeNodes.get,
                  renderItem,
                  expandedKeys = expandedGroups.get,
                  onToggle = expandedGroups.set,
                  dragDropScope = if (props.readonly) js.undefined else "obs-tree",
                  onDragDrop = if (props.readonly) js.undefined else onDragDrop
                )
              )
            )
          } else EmptyVdom

        <.div(ExploreStyles.ObsTreeWrapper)(tree)
