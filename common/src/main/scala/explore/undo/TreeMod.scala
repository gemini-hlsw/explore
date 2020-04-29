// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import cats.implicits._
import mouse.boolean._
import cats.data.NonEmptyList
import scala.annotation.tailrec
import explore.util.tree._

class TreeMod[F[_], A, Id](hasId: Id => A => Boolean)
    extends IndexedColMod[F, Tree, NonEmptyList[Int], A, Id] {

  override def getById(tree: Tree[A], id: Id): Option[(A, NonEmptyList[Int])] = {
    def goNode(node: Node[A], accumIdx: NonEmptyList[Int]): Option[(A, NonEmptyList[Int])] =
      hasId(id)(node.value).fold(
        (node.value, accumIdx).some,
        goChildren(node.children, accumIdx.toList)
      )

    @tailrec
    def goChildren(
      children: List[Node[A]],
      accumIdx: List[Int] = List.empty,
      index:    Int = 0
    ): Option[(A, NonEmptyList[Int])] = children match {
      case Nil => none
      case h :: t =>
        goNode(h, NonEmptyList.ofInitLast(accumIdx, index)) match {
          case Some(r) => r.some
          case None    => goChildren(t, accumIdx, index + 1)
        }
    }

    goChildren(tree.children)
  }

  override def removeWithIdx(tree: Tree[A], idx: NonEmptyList[Int]): Tree[A] = {

    def goChildren(children: List[Node[A]], remainingIdx: NonEmptyList[Int]): List[Node[A]] = {
      val idx = remainingIdx.head
      children.get(idx.toLong).fold(List.empty[Node[A]]) { node =>
        val init = children.take(idx)
        val tail = children.drop(idx + 1)
        remainingIdx.tail.toNel.fold(init ++ tail)(nelTail =>
          init ++ (Node(
            node.value,
            goChildren(node.children, nelTail)
          ) +: tail)
        )
      }
    }

    Tree(goChildren(tree.children, idx))
  }

  override def insertWithIdx(tree: Tree[A], idx: NonEmptyList[Int], a: A): Tree[A] = {
    def goChildren(children: List[Node[A]], remainingIdx: NonEmptyList[Int]): List[Node[A]] = {
      val idx  = remainingIdx.head
      val init = children.take(idx)
      val tail = children.drop(idx)
      remainingIdx.tail.toNel.fold(init ++ (Node(a) +: tail))(nelTail =>
        tail.headOption.fold(init ++ tail) { node =>
          init ++ (Node(
            node.value,
            goChildren(node.children, nelTail)
          ) +: tail.tail)
        }
      )
    }

    Tree(goChildren(tree.children, idx))
  }
}
