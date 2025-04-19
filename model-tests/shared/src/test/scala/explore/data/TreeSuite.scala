// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.data

import cats.kernel.laws.discipline.EqTests
import cats.laws.discipline.FunctorTests
import explore.data.tree.Node
import explore.data.tree.Tree
import explore.model.arb.all.*
import munit.DisciplineSuite

class TreeSuite extends DisciplineSuite {
  checkAll("Eq[Tree]", EqTests[Tree[Int]].eqv)
  checkAll("Eq[Node]", EqTests[Node[Int]].eqv)
  checkAll("Functor[Tree]", FunctorTests[Tree].functor[Int, Int, Int])
  checkAll("Functor[Node]", FunctorTests[Node].functor[Int, Int, Int])
}
