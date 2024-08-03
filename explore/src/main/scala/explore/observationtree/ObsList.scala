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
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.data.tree.KeyedIndexedTree
import explore.data.tree.Node as ExploreNode
import explore.data.tree.Tree as ExploreTree
import explore.model.AppContext
import explore.model.Focused
import explore.model.GroupTree
import explore.model.Observation
import explore.model.ObservationExecutionMap
import explore.model.enums.AppTab
import explore.model.reusability.given
import explore.model.syntax.all.*
import explore.tabs.DeckShown
import explore.undo.UndoSetter
import explore.undo.Undoer
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsStatus
import lucuma.core.model.Group
import lucuma.core.model.Program
import lucuma.core.model.Target
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
  observations:      UndoSetter[ObservationList],
  obsExecutionTimes: ObservationExecutionMap,
  undoer:            Undoer,
  programId:         Program.Id,
  focusedObs:        Option[Observation.Id],
  focusedTarget:     Option[Target.Id],
  focusedGroup:      Option[Group.Id],
  setSummaryPanel:   Callback,
  groups:            UndoSetter[GroupTree],
  expandedGroups:    View[Set[Group.Id]],
  deckShown:         View[DeckShown],
  readonly:          Boolean
) extends ReactFnProps(ObsList.component)

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
    Iso[GroupTree, List[Node[GroupTree.Value]]](groups =>
      def createNode(node: GroupTree.Node): Node[GroupTree.Value] =
        val isSystemGroup: Boolean    = node.value.toOption.exists(_.system)
        val isCalibrationObs: Boolean = node.value.left.toOption
          .flatMap(obs => allObservations.getValue(obs.id))
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

      groups.toTree.children.map(createNode)
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

        val expandedGroups = props.expandedGroups.as(groupTreeIdLens)

        def onDragDrop(e: Tree.DragDropEvent[GroupTree.Value]): Callback =
          // If PrimeReact fixes https://github.com/primefaces/primereact/issues/6976,
          // then we can remove the 2nd check and set `droppable` to false for groups.
          if (e.dropNode.exists(node => node.data.isLeft || node.data.toOption.exists(_.system)))
            Callback.empty
          else {
            val dragNode   = e.dragNode.data
            val dropNodeId = e.dropNode.flatMap(_.data.id.toOption)

            val dropIndex =
              NonNegShort.from(e.dropIndex.toShort).getOrElse(NonNegShort.unsafeFrom(0))

              // Group updates are done through the `treeNodes.set`, but obs updates have to be done separately
            val obsEdit: Callback = dragNode.left.toOption
              .map: obs =>
                props.observations.model
                  .mod(
                    _.updatedValueWith(
                      obs.id,
                      Observation.groupId
                        .replace(dropNodeId)
                        .andThen(Observation.groupIndex.replace(dropIndex))
                    )
                  )
              .getOrEmpty

            dragNode.id
              .fold(
                obsId => ObsQueries.moveObservation[IO](obsId, dropNodeId, dropIndex.some),
                groupId => GroupQueries.moveGroup[IO](groupId, dropNodeId, dropIndex.some)
              )
              .runAsync *>
              treeNodes.value.set(e.value.toList) *>
              obsEdit *>
              // Open the group we moved to
              dropNodeId.map(id => props.expandedGroups.mod(_ + id)).getOrEmpty
          }

        def renderItem(node: GroupTree.Value, options: TreeNodeTemplateOptions): VdomNode =
          node match
            case Left(GroupTree.Obs(id)) =>
              props.observations.get
                .getValue(id)
                .map: obs =>
                  val selected: Boolean = props.focusedObs.contains_(id)

                  <.a(
                    ^.id        := s"obs-list-${id.toString}",
                    ^.href      := ctx.pageUrl(
                      AppTab.Observations,
                      props.programId,
                      Focused.singleObs(id, props.focusedTarget)
                    ),
                    // Disable link dragging to enable tree node dragging
                    ^.draggable := false,
                    ExploreStyles.ObsItem |+| ExploreStyles.SelectedObsItem.when_(selected),
                    ^.onClick ==> linkOverride(
                      setObs(props.programId, id.some, ctx)
                    )
                  )(
                    ObsBadge(
                      obs,
                      props.obsExecutionTimes.getPot(id).map(_.programTimeEstimate),
                      ObsBadge.Layout.ObservationsTab,
                      selected = selected,
                      setStatusCB = obsEditStatus(id)
                        .set(props.observations)
                        .compose((_: ObsStatus).some)
                        .some,
                      setActiveStatusCB = obsActiveStatus(id)
                        .set(props.observations)
                        .compose((_: ObsActiveStatus).some)
                        .some,
                      setSubtitleCB = obsEditSubtitle(id)
                        .set(props.observations)
                        .compose((_: Option[NonEmptyString]).some)
                        .some,
                      deleteCB = obsExistence(
                        id,
                        o => setObs(props.programId, o.some, ctx)
                      )
                        .mod(props.observations)(obsListMod.delete)
                        .showToastCB(s"Deleted obs ${id.show}"),
                      cloneCB = cloneObs(
                        props.programId,
                        id,
                        props.observations.get.length,
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
            case Right(group)            =>
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
                deleteCB = groupExistence(
                  group.id,
                  g => setGroup(props.programId, g.some, ctx)
                )
                  .mod(props.groups)(groupTreeMod.delete)
                  .showToastCB(s"Deleted group ${group.id.show}"),
                isEmpty = isEmpty,
                readonly = props.readonly || group.system
              )

        val expandFocusedGroup: Callback = props.expandedGroups.mod(_ ++ props.focusedGroup)

        val tree =
          if (props.deckShown.get === DeckShown.Shown) {
            React.Fragment(
              <.div(ExploreStyles.TreeToolbar)(
                if (props.readonly) EmptyVdom
                else
                  val isSystemGroupFocused: Boolean =
                    props.focusedGroup
                      .flatMap: groupId =>
                        props.groups.get.getNodeAndIndexByKey(groupId.asRight)
                      .exists(_._1.value.toOption.exists(_.system))

                  React.Fragment(
                    Button(
                      severity = Button.Severity.Success,
                      icon = Icons.New,
                      label = "Obs",
                      disabled = adding.get.value || isSystemGroupFocused,
                      loading = adding.get.value,
                      onClick = insertObs(
                        props.programId,
                        // Set the focused group as the new obs parent if it is selected
                        props.focusedGroup,
                        props.observations.get.length,
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
                      onClick = insertGroup(
                        props.programId,
                        // Set the focused group as the new group parent if it is selected
                        props.focusedGroup,
                        props.groups,
                        adding,
                        ctx
                      ).runAsync *> expandFocusedGroup
                    ).mini.compact
                  )
                ,
                <.div(
                  ExploreStyles.ObsTreeButtons,
                  Button(
                    severity = Button.Severity.Secondary,
                    outlined = true,
                    disabled = false,
                    icon = Icons.ArrowLeftFromLine,
                    clazz = ExploreStyles.ObsTreeHideShow,
                    onClick = props.deckShown.mod(_.flip)
                  ).mini.compact,
                  if (props.readonly) EmptyVdom
                  else
                    UndoButtons(props.undoer, size = PlSize.Mini, disabled = adding.get.value)
                )
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
              <.div(
                ^.overflow := "auto",
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
