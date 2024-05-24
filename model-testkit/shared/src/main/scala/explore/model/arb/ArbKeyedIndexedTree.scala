// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import cats.kernel.Eq
import explore.data.tree.KeyedIndexedTree
import explore.data.tree.Tree
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Cogen.*

trait ArbKeyedIndexedTree {
  implicit def keyedIndexedTreeArb[K, A](implicit
    eqK:      Eq[K],
    arbKeyFn: Arbitrary[A => K],
    arbTree:  Arbitrary[Tree[A]]
  ): Arbitrary[KeyedIndexedTree[K, A]] =
    Arbitrary[KeyedIndexedTree[K, A]] {
      for {
        tree   <- arbitrary[Tree[A]]
        getKey <- arbitrary[A => K]
      } yield KeyedIndexedTree.fromTree(tree, getKey)
    }

  implicit def keyedIndexedTreeCogen[K, A](implicit
    cogenTreeA: Cogen[Tree[A]]
  ): Cogen[KeyedIndexedTree[K, A]] =
    Cogen[Tree[A]].contramap(_.toTree)
}

object ArbKeyedIndexedTree extends ArbKeyedIndexedTree
