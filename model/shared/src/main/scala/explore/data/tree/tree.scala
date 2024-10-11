// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.data.tree

import cats.*
import cats.syntax.all.*
import monocle.Focus

case class Tree[A](children: List[Node[A]]) {
  def map[B](f: A => B): Tree[B] =
    Tree(children.map(_.map(f)))
}

case class Node[A](value: A, children: List[Node[A]] = List.empty) {
  def map[B](f: A => B): Node[B] =
    Node(f(value), children.map(_.map(f)))
}

object Tree {
  def empty[A]: Tree[A]                     = Tree(List.empty)
  def apply[A](children: Node[A]*): Tree[A] = Tree(children.toList)

  implicit def eqTree[A: Eq]: Eq[Tree[A]] = Eq.by(_.children)

  implicit val functorTree: Functor[Tree] =
    new Functor[Tree] {
      override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa.map(f)
    }
}

object Node {
  def apply[A](value: A, children: Node[A]*): Node[A] = Node(value, children.toList)

  def value[A] = Focus[Node[A]](_.value)

  implicit def eqNode[A: Eq]: Eq[Node[A]] =
    Eq.instance((node1, node2) => node1.value === node2.value && node1.children === node2.children)

  implicit val functorNode: Functor[Node] =
    new Functor[Node] {
      override def map[A, B](fa: Node[A])(f: A => B): Node[B] = fa.map(f)
    }
}

// Version with different types for internal and leaf nodes. I don't think this would work easily with tree component.
// @Lenses case class Tree[A, B](children: List[Node[A, B]])
// sealed trait Node[A, B]
// object Node {
//   @Lenses case class Internal[A, B](value: A, children: List[Node[A, B]] = List.empty)
//       extends Node[A, B]
//   @Lenses case class Leaf[A, B](value: B) extends Node[A, B]
// }

// object Tree {
//   def empty[A, B]: Tree[A, B] = Tree(List.empty)
//   def apply[A, B](children: Node[A, B]*): Tree[A, B] = Tree(children.toList)
// }
