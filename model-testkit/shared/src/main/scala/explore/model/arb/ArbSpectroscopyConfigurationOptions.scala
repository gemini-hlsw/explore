// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.SpectroscopyConfigurationOptions
import lucuma.core.math.arb.ArbAngle._
import lucuma.core.math.arb.ArbWavelength._
import lucuma.core.util.arb.ArbEnumerated._
import lucuma.core.util.arb.ArbGid._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Cogen._
// these need to be at the end to avoid diverging implicit expansion problems
import eu.timepit.refined.scalacheck.numeric._
import lucuma.core.math.Wavelength
import eu.timepit.refined._
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosDouble
import eu.timepit.refined.numeric.Positive
import explore.model.enum.FocalPlaneOptions
import lucuma.core.math.Angle
import explore.model.enum.SpectroscopyCapabilities
import scala.math.BigDecimal

trait ArbSpectroscopyConfigurationOptions {

  implicit val spectroscopyConfigurationurationOptionsArb
    : Arbitrary[SpectroscopyConfigurationOptions] = Arbitrary {
    for {
      wv  <- arbitrary[Option[Wavelength]]
      rp  <- arbitrary[Option[PosInt]]
      sn  <- arbitrary[Option[PosDouble]]
      sna <- arbitrary[Option[Wavelength]]
      wr  <- arbitrary[Option[Wavelength]]
      sm  <- arbitrary[FocalPlaneOptions]
      fa  <- arbitrary[Option[Angle]]
      sc  <- arbitrary[Option[SpectroscopyCapabilities]]
    } yield SpectroscopyConfigurationOptions(
      wv,
      rp,
      sn.flatMap(v => refineV[Positive](BigDecimal(v.value)).toOption),
      sna,
      wr,
      sm,
      fa,
      sc
    )
  }

  implicit val spectroscopyConfigurationOptionsCogen: Cogen[SpectroscopyConfigurationOptions] =
    Cogen[
      (
        Option[Wavelength],
        Option[Int],
        Option[BigDecimal],
        Option[Wavelength],
        Option[Wavelength],
        FocalPlaneOptions,
        Option[Angle],
        Option[SpectroscopyCapabilities]
      )
    ]
      .contramap(cs =>
        (cs.wavelength,
         cs.resolutionPower.map(_.value),
         cs.signalToNoise.map(_.value),
         cs.signalToNoiseAt,
         cs.wavelengthRange,
         cs.slitMode,
         cs.focalPlaneAngle,
         cs.capabilities
        )
      )
}

object ArbSpectroscopyConfigurationOptions extends ArbSpectroscopyConfigurationOptions
