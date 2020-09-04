// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.data

import munit.DisciplineSuite
import cats.kernel.laws.discipline.EqTests
import explore.model.arb.all._
import explore.data.tree.KeyedIndexedTree

class KeyedIndexedTreeSuite extends DisciplineSuite {
  checkAll("Eq[KeyedIndexedTree]", EqTests[KeyedIndexedTree[Int, (Int, String)]].eqv)
}
