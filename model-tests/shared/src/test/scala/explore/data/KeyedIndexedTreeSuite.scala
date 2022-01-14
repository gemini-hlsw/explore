// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.data

import cats.kernel.laws.discipline.EqTests
import explore.data.tree.KeyedIndexedTree
import explore.model.arb.all._
import munit.DisciplineSuite

class KeyedIndexedTreeSuite extends DisciplineSuite {
  checkAll("Eq[KeyedIndexedTree]", EqTests[KeyedIndexedTree[Int, (Int, String)]].eqv)
}
