// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import coulomb.scalacheck.ArbQuantity._
import coulomb.Quantity
import explore.model.SpectroscopyConfigurationOptions
import lucuma.core.math.arb.ArbAngle._
import lucuma.core.util.arb.ArbEnumerated._
import lucuma.core.util.arb.ArbGid._
import lucuma.core.math.Angle
import lucuma.core.math.units.Micrometer
import lucuma.core.enum.FocalPlane
import lucuma.core.enum.SpectroscopyCapabilities
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Cogen._
// these need to be at the end to avoid diverging implicit expansion problems
import eu.timepit.refined.scalacheck.numeric._
import eu.timepit.refined._
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosDouble
import eu.timepit.refined.numeric.Positive
import scala.math.BigDecimal

trait ArbSpectroscopyConfigurationOptions {

  implicit val spectroscopyConfigurationurationOptionsArb
    : Arbitrary[SpectroscopyConfigurationOptions] = Arbitrary {
    for {
      wv  <- arbitrary[Option[Quantity[BigDecimal, Micrometer]]]
      rp  <- arbitrary[Option[PosInt]]
      sn  <- arbitrary[Option[PosDouble]]
      sna <- arbitrary[Option[Quantity[BigDecimal, Micrometer]]]
      wr  <- arbitrary[Option[Quantity[BigDecimal, Micrometer]]]
      sm  <- arbitrary[Option[FocalPlane]]
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
        Option[Quantity[BigDecimal, Micrometer]],
        Option[Int],
        Option[BigDecimal],
        Option[Quantity[BigDecimal, Micrometer]],
        Option[Quantity[BigDecimal, Micrometer]],
        Option[FocalPlane],
        Option[Angle],
        Option[SpectroscopyCapabilities]
      )
    ]
      .contramap(cs =>
        (cs.wavelengthQ,
         cs.resolution.map(_.value),
         cs.signalToNoise.map(_.value),
         cs.signalToNoiseAtQ,
         cs.wavelengthCoverageQ,
         cs.focalPlane,
         cs.focalPlaneAngle,
         cs.capabilities
        )
      )
}

object ArbSpectroscopyConfigurationOptions extends ArbSpectroscopyConfigurationOptions
