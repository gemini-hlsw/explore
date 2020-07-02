// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// package explore.observationtree

// import scala.annotation.tailrec
// import scala.scalajs.js
// import scala.scalajs.js.JSON

// import cats.effect.IO
// import cats.implicits._
// import cats.kernel.Eq
// import crystal.implicits._
// import crystal.react.ModState
// import crystal.react.implicits._
// import explore.AppCtx
// import explore.AppMain
// import explore.components.graphql.SubscriptionRenderMod
// import explore.components.undo.UndoRegion
// import explore.data.tree._
// import explore.implicits._
// import explore.model.Constraints
// import explore.model.RootModel
// import explore.model.reusability._
// import explore.undo.TreeMod
// import explore.undo.Undoer
// import gem.Observation
// import gem.ProgramId
// import gsp.math.Index
// import japgolly.scalajs.react.MonocleReact._
// import japgolly.scalajs.react._
// import japgolly.scalajs.react.vdom.html_<^._
// import monocle.function.Cons.headOption
// import monocle.macros.Lenses
// import mouse.boolean._
// import react.atlasKit.tree.{ Tree => AtlasTree }
// import react.common.ReactProps
// import react.semanticui.elements.button.Button

// import js.annotation._
// import js.JSConverters._

// final case class TreeComp[A, Id](
//   tree:              Tree[A],
//   getId:             A => Id,
//   idFromItemId:      AtlasTree.ItemId => Id,
//   treeMod:           TreeMod[IO, A, Id],
//   render:            AtlasTree.RenderItemParams[A] => VdomNode
// )(implicit val eqId: Eq[Id])
//     extends ReactProps[TreeComp[Any, Any]](TreeComp.component)

// object TreeComp {
//   type Props[A, Id] = TreeComp[A, Id]

//   // Convert a (recursive) explore.data.Tree to a (flat) AtlasKit Tree Data.
//   private def treeToData[A, Id](
//     tree:         Tree[A],
//     getId:        A => String,
//     collapsedIds: Set[String]
//   ): AtlasTree.Data[A] = {
//     def valueChildrenToData(
//       value:    Option[A],
//       children: List[Node[A]],
//       accum:    List[(String, AtlasTree.Item[A])] = List.empty
//     ): List[(String, AtlasTree.Item[A])] = {
//       val id = value.map(getId).getOrElse("")
//       children.foldLeft(
//         accum :+
//           (id -> AtlasTree.Item[A](
//             id,
//             children.map(node => getId(node.value)),
//             hasChildren = children.nonEmpty,
//             isExpanded = !collapsedIds.contains(id),
//             isChildrenLoading = false,
//             value.orUndefined
//           ))
//       ) {
//         case (nodeAccum, node) => valueChildrenToData(node.value.some, node.children, nodeAccum)
//       }
//     }

//     AtlasTree.Data(
//       rootId = "",
//       items = valueChildrenToData(none, tree.children).toMap
//     )
//   }

//   // Given a tree index (Option[ParentId], ChildIndex), get the Option[Item] with that index.
//   // TODO Let's hava a tree where we can find a node by its index in constant time.
//   private def treeGet[A, Id: Eq](
//     tree:     Tree[A],
//     getId:    A => Id
//   )(parentId: Option[Id], index: Int): Option[Node[A]] = {
//     def go(value: Option[A], children: List[Node[A]]): Option[Node[A]] =
//       (value.map(getId) === parentId).fold(
//         children.get(index.toLong),
//         goChildren(children)
//       )

//     def goChildren(children: List[Node[A]]): Option[Node[A]] =
//       children match {
//         case Nil          => none
//         case head :: next => go(head.value.some, head.children).orElse(goChildren(next))
//       }

//     go(none, tree.children)
//   }

//   @Lenses
//   final case class State[A](tree: Tree[A], collapsedIds: Set[AtlasTree.ItemId] = Set.empty)

//   class Backend[A, Id]($ : BackendScope[Props[A, Id], State[A]]) {

//     // use onDragStart to get Item.id (instead of looking it up by position)

//     val onCollapse =
//       (itemId: AtlasTree.ItemId, _: AtlasTree.Path) =>
//         $.modStateL(State.collapsedIds[A])(_ + itemId)

//     val onExpand =
//       (itemId: AtlasTree.ItemId, _: AtlasTree.Path) =>
//         $.modStateL(State.collapsedIds[A])(_ - itemId)

//     def onDragEnd(
//       tree:     Tree[A],
//       modState: ModState[IO, Tree[A]],
//       setter:   Undoer.Setter[IO, Tree[A]]
//     )(source:   AtlasTree.Position, destination: Option[AtlasTree.Position]): Callback =
//       $.props >>= { props =>
//         implicit val eqId = props.eqId

//         def pos2Index(pos: AtlasTree.Position): TreeMod.Index[Id] =
//           (pos.parentId.some.filterNot(_.isEmpty).map(props.idFromItemId), pos.index)

//         (for {
//           dest <- destination
//           item <- (treeGet(tree, props.getId) _).tupled(
//                     pos2Index(source)
//                   )
//         } yield {

//           // println(s"Dragged Item: [$item]")

//           val getSet   =
//             props.treeMod.pos
//               .withId(props.getId(item.value))
//           val newValue = (item, pos2Index(dest))
//           val modify   =
//             setter
//               .set(tree,
//                    getSet.getter.get,
//                    { value: props.treeMod.Existence =>
//                      (modState.apply _).compose(getSet.setter.set)(value)
//                    }
//               ) _
//           modify(newValue.some).runInCB
//         }).getOrEmpty
//       }

//     def render(props: Props[A, Id], state: State[A]): VdomElement =
//       UndoRegion[Tree[A]] { undoCtx =>
//         // println(state.tree)

//         <.div(
//           <.div(
//             ^.height := "750px",
//             ^.width := "1000px",
//             ^.paddingTop := "30px",
//             ^.overflow.auto // overflow.auto or overflow.scroll needed to drag'n'drop intermediate nodes
//           )(
//             AtlasTree[A](
//               tree = treeToData(state.tree, props.getId.andThen(_.toString), state.collapsedIds),
//               renderItem = props.render,
//               onCollapse = onCollapse,
//               onExpand = onExpand,
//               onDragEnd = onDragEnd(state.tree,
//                                     ($.modStateIn[IO] _).compose(State.tree.modify),
//                                     undoCtx.setter
//               ) _,
//               isDragEnabled = true
//               // isNestingEnabled = true // Work this into facade. Seems to trigger onDragEnd with defined dest but with undefined index.
//             )
//           ),
//           <.div(
//             Button(onClick = undoCtx.undo(state.tree).runInCB, disabled = undoCtx.undoEmpty)(
//               "Undo"
//             ),
//             Button(onClick = undoCtx.redo(state.tree).runInCB, disabled = undoCtx.redoEmpty)(
//               "Redo"
//             )
//           )
//         )
//       }
//   }

//   def componentBuilder[A, Id] =
//     ScalaComponent
//       .builder[Props[A, Id]]
//       .initialStateFromProps(props => State[A](props.tree))
//       .backend(new Backend(_))
//       .renderBackend
//       .build

//   val component = componentBuilder[Any, Any]
// }
