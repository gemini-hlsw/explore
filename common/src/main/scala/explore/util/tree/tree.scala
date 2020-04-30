// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.util.tree

import monocle.macros.Lenses

@Lenses final case class Tree[A](children: List[Node[A]])
@Lenses final case class Node[A](value:    A, children: List[Node[A]] = List.empty)

object Tree {
  def empty[A]: Tree[A] = Tree(List.empty)
  def apply[A](children: Node[A]*): Tree[A] = Tree(children.toList)
}

object Node {
  def apply[A](a: A, children: Node[A]*): Node[A] = Node(a, children.toList)
}
