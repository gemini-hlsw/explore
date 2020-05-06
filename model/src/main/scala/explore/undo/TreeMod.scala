// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import cats.implicits._
import cats.kernel.Eq
import explore.util.tree._
import monocle.Getter
import monocle.Lens
import mouse.boolean._
import scala.annotation.tailrec

object TreeMod                                               {
  type Index[Id] =
    (Option[Id], Int) // (Parent's Id (unless it's the root), Position within parent's children)
}

class TreeMod[F[_], A, Id: Eq](protected val idLens: Lens[A, Id])
    extends IndexedColMod[F, Tree, TreeMod.Index[Id], A, Id] {

  protected def hasId(id: Id)(a: A): Boolean = Eq[Id].eqv(id, idLens.get(a))

  /*
  // TODO Convert all the logic to more optic'y way, with compositions.
  val nodeListGetterForId: Id => Getter[List[Node[A]], Option[(A, TreeMod.Index[Id])]] =
    (_ /*id*/: Id) => ???

  def getterForId1(id: Id): Getter[Tree[A], Option[(A, TreeMod.Index[Id])]] =
    Tree.children.composeGetter(nodeListGetterForId(id))
   */

  def getterForId(id:              Id): Getter[Tree[A], Option[(A, TreeMod.Index[Id])]] =
    Getter[Tree[A], Option[(A, TreeMod.Index[Id])]] { tree =>
      def goNode(node: Node[A], idx: TreeMod.Index[Id]): Option[(A, TreeMod.Index[Id])] =
        hasId(id)(node.value).fold(
          (node.value, idx).some,
          goChildren(node.children, idLens.get(node.value).some)
        )

      @tailrec
      def goChildren(
        children: List[Node[A]],
        parentId: Option[Id] = None,
        index:    Int = 0
      ): Option[(A, TreeMod.Index[Id])] =
        children match {
          case Nil    => none
          case h :: t =>
            goNode(h, (parentId, index)) match {
              case Some(r) => r.some
              case None    => goChildren(t, parentId, index + 1)
            }
        }

      goChildren(tree.children)
    }

  // format: off
  override def removeWithIdx(tree: Tree[A], idx: TreeMod.Index[Id]): Tree[A] = {
    def goChildren(children: List[Node[A]], parentId: Option[Id] = None): List[Node[A]] =
      (idx._1.forall(id => parentId.exists(pid => Eq[Id].eqv(id, pid)))).fold(
        children.take(idx._2) ++ children.drop(idx._2 + 1),
        children.map(node => Node(node.value, goChildren(node.children, idLens.get(node.value).some)))
      )

    Tree(goChildren(tree.children))
  }

  override def insertWithIdx(tree: Tree[A], idx: TreeMod.Index[Id], a: A): Tree[A] = {
    def goChildren(children: List[Node[A]], parentId: Option[Id] = None): List[Node[A]] =
      (idx._1.forall(id => parentId.exists(pid => Eq[Id].eqv(id, pid)))).fold(
        children.take(idx._2) ++ (Node(a) +: children.drop(idx._2)),
        children.map(node => Node(node.value, goChildren(node.children, idLens.get(node.value).some)))
      )

    Tree(goChildren(tree.children))
  }
  // format: on
}
