// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import cats.data.NonEmptyList
import cats.laws.discipline.arbitrary.*
import cats.syntax.all.*
import eu.timepit.refined.scalacheck.numeric.*
import explore.model.BasicConfiguration
import explore.model.itc.CoverageCenterWavelength
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.util.arb.ArbEnumerated.*
import lucuma.core.enums.*
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbBasicConfiguration {
  import ArbWavelength.*

  given Arbitrary[BasicConfiguration.GmosNorthLongSlit] =
    Arbitrary[BasicConfiguration.GmosNorthLongSlit](
      for {
        grating <- arbitrary[GmosNorthGrating]
        filter  <- arbitrary[Option[GmosNorthFilter]]
        fpu     <- arbitrary[GmosNorthFpu]
        cw      <- arbitrary[Wavelength]
      } yield BasicConfiguration.GmosNorthLongSlit(
        grating,
        filter,
        fpu,
        CoverageCenterWavelength(cw)
      )
    )

  given Arbitrary[BasicConfiguration.GmosSouthLongSlit] =
    Arbitrary[BasicConfiguration.GmosSouthLongSlit](
      for {
        grating <- arbitrary[GmosSouthGrating]
        filter  <- arbitrary[Option[GmosSouthFilter]]
        fpu     <- arbitrary[GmosSouthFpu]
        cw      <- arbitrary[Wavelength]
      } yield BasicConfiguration.GmosSouthLongSlit(
        grating,
        filter,
        fpu,
        CoverageCenterWavelength(cw)
      )
    )

  given Arbitrary[BasicConfiguration] = Arbitrary[BasicConfiguration](
    Gen.oneOf(
      arbitrary[BasicConfiguration.GmosNorthLongSlit],
      arbitrary[BasicConfiguration.GmosSouthLongSlit]
    )
  )

  given Cogen[BasicConfiguration.GmosNorthLongSlit] =
    Cogen[
      (GmosNorthGrating, Option[GmosNorthFilter], GmosNorthFpu)
    ]
      .contramap(o =>
        (
          o.grating,
          o.filter,
          o.fpu
        )
      )

  given Cogen[BasicConfiguration.GmosSouthLongSlit] =
    Cogen[
      (GmosSouthGrating, Option[GmosSouthFilter], GmosSouthFpu)
    ]
      .contramap(o =>
        (
          o.grating,
          o.filter,
          o.fpu
        )
      )

  given Cogen[BasicConfiguration] =
    Cogen[Either[BasicConfiguration.GmosNorthLongSlit, BasicConfiguration.GmosSouthLongSlit]]
      .contramap {
        case n: BasicConfiguration.GmosNorthLongSlit => n.asLeft
        case s: BasicConfiguration.GmosSouthLongSlit => s.asRight
      }

}

object ArbBasicConfiguration extends ArbBasicConfiguration
