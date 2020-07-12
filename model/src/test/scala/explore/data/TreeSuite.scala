// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.data

import cats.implicits._
import munit.DisciplineSuite
import cats.kernel.laws.discipline.EqTests
import explore.model.arb.all._
import explore.data.tree.Tree
import cats.laws.discipline.FunctorTests
import explore.data.tree.Node

class TreeSuite extends DisciplineSuite {
  checkAll("Eq[Tree]", EqTests[Tree[Int]].eqv)
  checkAll("Eq[Node]", EqTests[Node[Int]].eqv)
  checkAll("Functor[Tree]", FunctorTests[Tree].functor[Int, Int, Int])
  checkAll("Functor[Node]", FunctorTests[Node].functor[Int, Int, Int])
}
