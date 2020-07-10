// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Cogen._
import explore.data.tree.Tree
import explore.data.tree.KeyedIndexedTree
import cats.kernel.Eq

trait ArbKeyedIndexedTree {
  implicit def keyedIndexedTreeArb[K, A](implicit
    eqK:      Eq[K],
    arbKeyFn: Arbitrary[A => K],
    arbTree:  Arbitrary[Tree[A]]
  ) =
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
