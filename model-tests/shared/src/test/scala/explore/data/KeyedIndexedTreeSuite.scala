// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.data

import cats.data.NonEmptyList
import cats.kernel.laws.discipline.EqTests
import cats.laws.discipline.arbitrary.*
import explore.data.tree.KeyedIndexedTree
import explore.data.tree.Node
import explore.data.tree.Tree
import explore.model.arb.all.*
import munit.DisciplineSuite
import org.scalacheck.Prop.forAll

class KeyedIndexedTreeSuite extends DisciplineSuite {
  checkAll("Eq[KeyedIndexedTree]", EqTests[KeyedIndexedTree[Int, (Int, String)]].eqv)

  property(".mapElement(f).toTree == .toTree.map(f)") {
    forAll { (tree: KeyedIndexedTree[Int, String], f: String => Int) =>
      assertEquals(tree.mapElement(f, identity).toTree, tree.toTree.map(f))
    }
  }

  property("parentKeys includes all parents") {
    forAll { (nodes: NonEmptyList[String]) =>
      val distinctNodes = nodes.distinct
      // Tree with single node per level
      val treeNodes     =
        distinctNodes.foldLeft(List.empty[Node[String]])((acc, n) => Node(n, children = acc) :: Nil)
      val tree          = KeyedIndexedTree.fromTree(Tree(treeNodes), identity)

      assertEquals(tree.parentKeys(distinctNodes.head), distinctNodes.tail.reverse)
      assertEquals(tree.parentKeys(distinctNodes.last), Nil)
    }
  }
}
