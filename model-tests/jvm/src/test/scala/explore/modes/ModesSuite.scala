// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.effect.Resource
import cats.effect.IO
import cats.syntax.all._
import coulomb._
import coulomb.si.Second
import eu.timepit.refined._
import eu.timepit.refined.numeric._
import java.io.File
import lucuma.core.math.units._
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import munit.CatsEffectSuite
import spire.math.Rational

class ModesSuite extends CatsEffectSuite {
  val csvPath = IO {
    val f =
      new File(this.getClass().getClassLoader().getResource("instrument_matrix.csv").getFile())

    f.toPath()
  }
  val fixture = ResourceSuiteLocalFixture(
    "modes",
    Resource.make(csvPath.flatMap(ModesMatrix[IO]))(_ => IO.unit)
  )

  override def munitFixtures = List(fixture)

  test("csv loading") {
    IO(fixture()).map(_.matrix.foreach(println)) *>
      IO(fixture()).map(_.matrix.length).assertEquals(101)
  }

  test("spectroscopy selection") {
    IO(fixture())
      .map(
        _.spectroscopyModes(
          dwmin = ModeBandWidth(Rational.zero.withUnit[Nanometer]).some,
          dwmax = none,
          rmin = refineMV[Positive](BigDecimal(300)).some,
          dims = ModeSpatialDimension.One.some,
          coronograph = ModeCoronagraph.NoCoronagraph.some,
          mexp = refineMV[Positive](BigDecimal(1)).withUnit[Second].some,
          mos = ModeMOS.NoMOS.some,
          skysub = ModeSkysub.High.some,
          iqmax = ModeIQ(Angle.fromDoubleArcseconds(1.0)).some,
          fov = none,
          wlen = Wavelength.fromNanometers(800)
        )
      )
      .flatTap(_.traverse(IO.println))
      .map(_.length)
      .assertEquals(17)
  }
}
