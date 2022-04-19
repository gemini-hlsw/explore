// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import cats.syntax.all._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import explore.model.ScienceConfiguration
import explore.model.GmosNorthLongSlit
import explore.model.GmosSouthLongSlit
import lucuma.core.util.arb.ArbGid._
import lucuma.core.util.arb.ArbEnumerated._
import lucuma.core.enum.GmosNorthFilter
import lucuma.core.enum.GmosNorthDisperser
import lucuma.core.enum.GmosNorthFpu
import lucuma.core.enum.GmosSouthFilter
import lucuma.core.enum.GmosSouthDisperser
import lucuma.core.enum.GmosSouthFpu
import lucuma.core.math.Angle
import lucuma.core.math.arb.ArbAngle

trait ArbScienceConfiguration {
  import ArbAngle._

  implicit val arbGmosNorthLongSlit = Arbitrary[GmosNorthLongSlit] {
    for {
      filter    <- arbitrary[Option[GmosNorthFilter]]
      disperser <- arbitrary[GmosNorthDisperser]
      fpu       <- arbitrary[GmosNorthFpu]
      slitWidth <- arbitrary[Angle]
    } yield GmosNorthLongSlit(filter, disperser, fpu, slitWidth)
  }

  implicit val arbGmosSouthLongSlit = Arbitrary[GmosSouthLongSlit] {
    for {
      filter    <- arbitrary[Option[GmosSouthFilter]]
      disperser <- arbitrary[GmosSouthDisperser]
      fpu       <- arbitrary[GmosSouthFpu]
      slitWidth <- arbitrary[Angle]
    } yield GmosSouthLongSlit(filter, disperser, fpu, slitWidth)
  }

  implicit val arbScienceConfiguration = Arbitrary[ScienceConfiguration] {
    for {
      gmosNLS <- arbitrary[GmosNorthLongSlit]
      gmosSLS <- arbitrary[GmosSouthLongSlit]
      sc      <- Gen.oneOf(gmosNLS, gmosSLS)
    } yield sc
  }

  implicit val cogenGmosNorthLongSlit: Cogen[GmosNorthLongSlit] =
    Cogen[(Option[GmosNorthFilter], GmosNorthDisperser, GmosNorthFpu, Angle)]
      .contramap(o => (o.filter, o.disperser, o.fpu, o.slitWidth))

  implicit val cogenGmosSouthLongSlit: Cogen[GmosSouthLongSlit] =
    Cogen[(Option[GmosSouthFilter], GmosSouthDisperser, GmosSouthFpu, Angle)]
      .contramap(o => (o.filter, o.disperser, o.fpu, o.slitWidth))

  implicit val cogenScienceConfiguration: Cogen[ScienceConfiguration] =
    Cogen[Either[GmosNorthLongSlit, GmosSouthLongSlit]]
      .contramap {
        case n: GmosNorthLongSlit => n.asLeft
        case s: GmosSouthLongSlit => s.asRight
      }

}

object ArbScienceConfiguration extends ArbScienceConfiguration
