// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import scala.collection.immutable.HashSet

import explore.components.ObsBadge
import explore.model.ExploreObservation
import explore.model.SiderealTarget
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import mouse.boolean._
import react.atlasKit.tree.{Tree => AtlasTree}
import react.common.ReactProps
import react.semanticui.elements.icon.Icon

// !!!! This approach does't seem to work using tree !!!!
// See https://github.com/atlassian/react-beautiful-dnd/issues/374
// and https://github.com/atlassian/react-beautiful-dnd/issues/1390
// We will attempt https://github.com/atlassian/react-beautiful-dnd/issues/374#issuecomment-569817782 in TargetObsList

final case class TargetTree(targets: List[SiderealTarget], observations: List[ExploreObservation])
    extends ReactProps[TargetTree](TargetTree.component)

object TargetTree {
  type Props = TargetTree

  @Lenses
  case class State(collapsedTargetIds: Set[String] = HashSet.empty)

  protected sealed trait TreeItem
  protected object TreeItem {
    final case class TargetItem(target: SiderealTarget, obsCount: Int) extends TreeItem
    final case class ObsItem(obs: ExploreObservation) extends TreeItem
  }

  class Backend[A, Id]($ : BackendScope[Props, State]) {
    import TreeItem._

    private def buildTreeData(
      targets:            List[SiderealTarget],
      observations:       List[ExploreObservation],
      collapsedTargetIds: Set[String]
    ): AtlasTree.Data[TreeItem] = {
      val obsByTarget = observations.groupBy(_.target)

      val root =
        "" ->
          AtlasTree.Item[TreeItem](
            "",
            targets.map(_.name),
            hasChildren = targets.nonEmpty,
            isExpanded = true,
            isChildrenLoading = false
          )

      val targetData = targets.map { target =>
        val targetObsIds = obsByTarget.get(target).fold(List.empty[String])(_.map(_.id.toString))

        target.name -> AtlasTree.Item[TreeItem](
          target.name,
          targetObsIds,
          hasChildren = targetObsIds.nonEmpty,
          isExpanded = !collapsedTargetIds.contains(target.name),
          isChildrenLoading = false,
          TargetItem(target, targetObsIds.length)
        )
      }

      val obsData = observations.map(obs =>
        obs.id.toString -> AtlasTree.Item[TreeItem](
          obs.id.toString,
          List.empty,
          hasChildren = false,
          isExpanded = false,
          isChildrenLoading = false,
          ObsItem(obs)
        )
      )

      AtlasTree.Data(
        rootId = "",
        items = ((root +: targetData) ++ obsData).toMap
      )
    }

    protected def renderItem(params: AtlasTree.RenderItemParams[TreeItem]): VdomNode = {
      def wrap(divAttrs: TagMod*)(element: VdomNode, overlay: VdomNode): VdomNode =
        <.div(
          <.div(^.position.absolute, ^.zIndex := "2", ^.pointerEvents.none)(divAttrs: _*)(overlay),
          <.div(divAttrs: _*)(element)
        )

      def handleClick: Callback =
        params.item.isExpanded
          .forall(identity)
          .fold(params.onCollapse(params.item.id), params.onExpand(params.item.id))

      def opIcon(hasChildren: Boolean) =
        hasChildren.fold(
          Icon(
            "chevron " + params.item.isExpanded.forall(identity).fold("down", "right")
          )(^.cursor.pointer, ^.onClick --> handleClick),
          Icon("chevron right")
        )

      def dragIcon =
        <.span(^.float.right, ^.pointerEvents.all, params.provided.dragHandleProps, Icon("sort"))

      <.div(params.provided.innerRef, params.provided.draggableProps)(
        <.div(^.margin := "5px")(
          params.item.data
            .map[VdomNode] {
              _ match {
                case TargetItem(target, obsCount) =>
                  <.span(
                    opIcon(obsCount > 0),
                    target.name,
                    <.span( /*^.display.none,*/ params.provided.dragHandleProps, "*"),
                    <.span(^.float.right, s"$obsCount Obs")
                  )
                case ObsItem(obs)                 =>
                  wrap(^.width := "225px")(
                    ObsBadge(obs, ObsBadge.Layout.ConfAndConstraints),
                    dragIcon
                  )
              }
            }
        )
      )
    }

    val onCollapse =
      (itemId: AtlasTree.ItemId, _: AtlasTree.Path) =>
        $.modStateL(State.collapsedTargetIds)(_ + itemId)

    val onExpand =
      (itemId: AtlasTree.ItemId, _: AtlasTree.Path) =>
        $.modStateL(State.collapsedTargetIds)(_ - itemId)

    def onDragEnd(source: AtlasTree.Position, destination: Option[AtlasTree.Position]): Callback =
      Callback.log(
        s"${scalajs.js.JSON.stringify(source)} ${destination.map(p => scalajs.js.JSON.stringify(p))}"
      )

    def render(props: Props, state: State): VdomElement =
      <.div(
        <.div(
          ^.height := "750px",
          ^.width := "260px",
          ^.paddingTop := "30px",
          ^.overflow.auto // overflow.auto or overflow.scroll needed to drag'n'drop intermediate nodes
        )(
          AtlasTree[TreeItem](
            tree = buildTreeData(props.targets, props.observations, state.collapsedTargetIds),
            renderItem = renderItem,
            offsetPerLevel = 20,
            onCollapse = onCollapse,
            onExpand = onExpand,
            onDragEnd = onDragEnd _,
            isDragEnabled = true,
            isNestingEnabled =
              true        // Work this into facade? Seems to trigger onDragEnd with defined dest but with undefined index.
          )
        )
      )

  }

  protected val component =
    ScalaComponent
      .builder[Props]
      .initialState(State())
      .backend(new Backend(_))
      .renderBackend
      .build

}
