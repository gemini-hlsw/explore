// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.data.tree.Node
import explore.data.tree.Tree
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Cogen.*
import org.scalacheck.Gen
import org.scalacheck.rng.Seed

trait ArbTree {

  def genTree[A](implicit arbA: Arbitrary[A]) =
    for {
      depth    <- Gen.choose(0, 6) // Limit max depth because of exponential blow
      children <- genChildren[A](depth)
    } yield Tree(children)

  def genChildren[A](maxDepth: Int)(implicit arbA: Arbitrary[A]): Gen[List[Node[A]]] =
    if (maxDepth == 0)
      List.empty[Node[A]]
    else
      Gen.lzy(
        for {
          siblings <- Gen.choose(0, 10) // Limit siblings because of exponential blow
          depths   <- Gen.listOfN(siblings, Gen.choose(0, maxDepth - 1))
          children <-
            Gen.sequence[List[Node[A]], Node[A]](depths.map(depth => genIntermediateNode[A](depth)))
        } yield children
      )

  def genIntermediateNode[A](maxDepth: Int)(implicit arbA: Arbitrary[A]): Gen[Node[A]] =
    if (maxDepth == 0)
      genLeaf[A]
    else
      Gen.lzy(
        for {
          a        <- arbitrary[A]
          children <- genChildren[A](maxDepth)
        } yield Node(a, children)
      )

  def genLeaf[A](implicit arbA: Arbitrary[A]): Gen[Node[A]] = arbitrary[A].map(a => Node(a))

  implicit def treeArb[A](implicit arbA: Arbitrary[A]): Arbitrary[Tree[A]] =
    Arbitrary[Tree[A]] {
      genTree[A]
    }

  implicit def nodeArb[A](implicit arbA: Arbitrary[A]): Arbitrary[Node[A]] =
    Arbitrary[Node[A]] {
      for {
        depth <- Gen.choose(0, 6) // Limit max depth because of exponential blow
        node  <- genIntermediateNode[A](depth)
      } yield node
    }

  implicit def cogenNode[A](implicit cogenA: Cogen[A]): Cogen[Node[A]] =
    Cogen[Node[A]]((s: Seed, node: Node[A]) =>
      node.children.foldLeft(cogenA.perturb(s, node.value))(cogenNode[A].perturb).next
    )

  implicit def cogenTree[A](implicit cogenA: Cogen[A]): Cogen[Tree[A]] =
    Cogen[List[Node[A]]].contramap(_.children)
}

object ArbTree extends ArbTree
