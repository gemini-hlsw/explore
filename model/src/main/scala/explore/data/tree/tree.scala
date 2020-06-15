// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.data.tree

import monocle.macros.Lenses

@Lenses final case class Tree[A](children: List[Node[A]])
@Lenses final case class Node[A](value: A, children: List[Node[A]] = List.empty)

object Tree {
  def empty[A]: Tree[A] = Tree(List.empty)
  def apply[A](children: Node[A]*): Tree[A] = Tree(children.toList)
}

object Node {
  def apply[A](a: A, children: Node[A]*): Node[A] = Node(a, children.toList)
}

// Version with different types for internal and leaf nodes. I don't think this would work easily with tree component.
// @Lenses final case class Tree[A, B](children: List[Node[A, B]])
// sealed trait Node[A, B]
// object Node {
//   @Lenses final case class Internal[A, B](value: A, children: List[Node[A, B]] = List.empty)
//       extends Node[A, B]
//   @Lenses final case class Leaf[A, B](value: B) extends Node[A, B]
// }

// object Tree {
//   def empty[A, B]: Tree[A, B] = Tree(List.empty)
//   def apply[A, B](children: Node[A, B]*): Tree[A, B] = Tree(children.toList)
// }
