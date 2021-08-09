// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all._
import coulomb._
import coulomb.refined._
import coulomb.si.Second
import eu.timepit.refined._
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import explore.model.enum.FocalPlane
import fs2.io.file.Path
import lucuma.core.enum.ImageQuality
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.math.units._
import munit.CatsEffectSuite
import spire.math.Rational

class ModesSuite extends CatsEffectSuite {
  val allModesPath = IO {
    Path(this.getClass().getClassLoader().getResource("instrument_matrix.csv").getPath)
  }

  val allModesFixture = ResourceSuiteLocalFixture(
    "modes",
    Resource.make(allModesPath.flatMap(ModesMatrix[IO]))(_ => IO.unit)
  )

  val spectroscopyPath = IO {
    Path(this.getClass().getClassLoader().getResource("instrument_spectroscopy_matrix.csv").getPath)
  }

  val spectroscopyModesFixture = ResourceSuiteLocalFixture(
    "spectrosocpy",
    Resource.make(spectroscopyPath.flatMap(SpectroscopyModesMatrix[IO]))(_ => IO.unit)
  )

  override def munitFixtures = List(allModesFixture, spectroscopyModesFixture)

  test("csv loading") {
    IO(allModesFixture()).map(_.matrix.length).assertEquals(101)
  }

  test("spectroscopy csv selection") {
    IO(spectroscopyModesFixture())
      .map(
        _.filtered(
          FocalPlane.SingleSlit.some,
          none,
          Some(ImageQuality.PointThree),
          Wavelength.fromNanometers(500),
          refineMV[Positive](1).some,
          BigDecimal(0).withRefinedUnit[NonNegative, Micrometer].some,
          Angle.fromDoubleArcseconds(1).some
        )
      )
      // .flatTap(_.traverse(IO.println))
      .map(_.length)
      .assertEquals(71)
  }

  test("spectroscopy csv loading") {
    IO(spectroscopyModesFixture())
      // .flatTap(_.matrix.traverse(IO.println))
      .map(_.matrix.length)
      .assertEquals(309)
  }

  test("spectroscopy selection old") {
    IO(allModesFixture())
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
      // .flatTap(_.traverse(IO.println))
      .map(_.length)
      .assertEquals(17)
  }
}
