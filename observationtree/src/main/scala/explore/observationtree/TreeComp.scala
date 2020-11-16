// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import scala.scalajs.js

import cats.effect.IO
import cats.syntax.all._
import crystal.react.ModState
import crystal.react.implicits._
import explore.components.undo.UndoRegion
import explore.data.tree.KeyedIndexedTree.Index
import explore.data.tree._
import explore.implicits._
import explore.undo.KITreeMod
import explore.undo.Undoer
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import react.atlasKit.tree.{ Tree => AtlasTree }
import react.common.ReactProps
import react.semanticui.elements.button.Button

import js.JSConverters._

final case class TreeComp[K, A](
  tree:          KeyedIndexedTree[K, A],
  keyToItemId:   K => AtlasTree.ItemId,
  keyFromItemId: AtlasTree.ItemId => K,
  treeMod:       KITreeMod[IO, A, K],
  render:        AtlasTree.RenderItemParams[A] => VdomNode
) extends ReactProps[TreeComp[Any, Any]](TreeComp.component)

object TreeComp {
  type Props[K, A] = TreeComp[K, A]

  // Convert a (recursive) KeyedIndexedTree to a (flat) AtlasKit Tree Data.
  private def treeToData[K, A](
    tree:             KeyedIndexedTree[K, A],
    keyToItemId:      K => AtlasTree.ItemId,
    collapsedItemIds: Set[AtlasTree.ItemId]
  ): AtlasTree.Data[A] = {
    def valueChildrenToData(
      keyValue: Option[(K, A)],
      children: List[Node[(K, A)]],
      accum:    List[(String, AtlasTree.Item[A])] = List.empty
    ): List[(String, AtlasTree.Item[A])] = {
      val getKey: ((K, A)) => String = keyToItemId.compose((kv: (K, A)) => kv._1)
      val key                        = keyValue.map(getKey).getOrElse("")
      children.foldLeft(
        accum :+
          (key -> AtlasTree.Item[A](
            key,
            children.map(node => getKey(node.value)),
            hasChildren = children.nonEmpty,
            isExpanded = !collapsedItemIds.contains(key),
            isChildrenLoading = false,
            keyValue.map(_._2).orUndefined
          ))
      ) { case (nodeAccum, node) =>
        valueChildrenToData(node.value.some, node.children, nodeAccum)
      }
    }

    AtlasTree.Data(
      rootId = "",
      items = valueChildrenToData(none, tree.toKeyedTree.children).toMap
    )
  }

  @Lenses
  final case class State[K, A](
    tree:             KeyedIndexedTree[K, A],
    collapsedItemIds: Set[AtlasTree.ItemId] = Set.empty
  )

  class Backend[K, A]($ : BackendScope[Props[K, A], State[K, A]]) {

    // use onDragStart to get Item.id (instead of looking it up by position)

    val onCollapse =
      (itemId: AtlasTree.ItemId, _: AtlasTree.Path) =>
        $.modStateL(State.collapsedItemIds[K, A])(_ + itemId)

    val onExpand =
      (itemId: AtlasTree.ItemId, _: AtlasTree.Path) =>
        $.modStateL(State.collapsedItemIds[K, A])(_ - itemId)

    def onDragEnd(
      tree:     KeyedIndexedTree[K, A],
      modState: ModState[IO, KeyedIndexedTree[K, A]],
      setter:   Undoer.Setter[IO, KeyedIndexedTree[K, A]]
    )(source:   AtlasTree.Position, destination: Option[AtlasTree.Position]): Callback =
      $.props >>= { props =>
        def pos2Index(pos: AtlasTree.Position): Index[K] =
          Index(pos.parentId.some.filterNot(_.isEmpty).map(props.keyFromItemId), pos.index)

        (for {
          dest      <- destination
          keyedNode <- tree.getKeyedNodeByIdx(pos2Index(source))
        } yield {

          // println(s"Dragged Item: [$item]")

          val getSet   =
            props.treeMod.pos
              .withKey(keyedNode._1)
          val newValue = (keyedNode._2, pos2Index(dest))
          val modify   =
            setter
              .set(tree,
                   getSet.getter.get,
                   { value: props.treeMod.ElemWithIndex =>
                     (modState.apply _).compose(getSet.adjuster.set)(value)
                   }
              ) _
          modify(newValue.some).runAsyncCB
        }).getOrEmpty
      }

    def render(props: Props[K, A], state: State[K, A]): VdomElement =
      UndoRegion[KeyedIndexedTree[K, A]] { undoCtx =>
        // println(state.tree)

        <.div(
          <.div(
            ^.height := "750px",
            ^.width := "1000px",
            ^.paddingTop := "30px",
            ^.overflow.auto // overflow.auto or overflow.scroll needed to drag'n'drop intermediate nodes
          )(
            AtlasTree[A](
              tree = treeToData(state.tree, props.keyToItemId, state.collapsedItemIds),
              renderItem = props.render,
              onCollapse = onCollapse,
              onExpand = onExpand,
              onDragEnd = onDragEnd(state.tree,
                                    ($.modStateIn[IO] _).compose(State.tree.modify),
                                    undoCtx.setter
              ) _,
              isDragEnabled = true
              // isNestingEnabled = true // Work this into facade. Seems to trigger onDragEnd with defined dest but with undefined index.
            )
          ),
          <.div(
            Button(onClick = undoCtx.undo(state.tree).runAsyncCB, disabled = undoCtx.undoEmpty)(
              "Undo"
            ),
            Button(onClick = undoCtx.redo(state.tree).runAsyncCB, disabled = undoCtx.redoEmpty)(
              "Redo"
            )
          )
        )
      }
  }

  def componentBuilder[K, A] =
    ScalaComponent
      .builder[Props[K, A]]
      .initialStateFromProps(props => State(props.tree))
      .backend(new Backend(_))
      .renderBackend
      .build

  val component = componentBuilder[Any, Any]
}
