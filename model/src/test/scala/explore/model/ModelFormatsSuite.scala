// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import lucuma.core.math.arb._
import explore.model.formats._
import munit.DisciplineSuite
import lucuma.core.optics.laws.discipline.FormatTests
import org.scalacheck.Gen
import org.scalacheck.Arbitrary._
import lucuma.core.math.Parallax

class ModelFormatsSuite extends DisciplineSuite {
  import ArbParallax._

  val parallaxMilliArcSeconds: Gen[String] =
    arbitrary[Parallax]
      .map(_.mas.value.toDouble.toString)

  checkAll("pxFormat", FormatTests(pxFormat).formatWith(parallaxMilliArcSeconds))
}
