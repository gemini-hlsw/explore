// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import cats.syntax.all._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import explore.model.ScienceModeBasic
import explore.model.GmosNorthLongSlit
import explore.model.GmosSouthLongSlit
import lucuma.core.util.arb.ArbGid._
import lucuma.core.util.arb.ArbEnumerated._
import lucuma.core.enum.GmosNorthFilter
import lucuma.core.enum.GmosNorthGrating
import lucuma.core.enum.GmosNorthFpu
import lucuma.core.enum.GmosSouthFilter
import lucuma.core.enum.GmosSouthGrating
import lucuma.core.enum.GmosSouthFpu

trait ArbScienceMode {

  implicit val arbGmosNorthLongSlit = Arbitrary[GmosNorthLongSlit] {
    for {
      grating <- arbitrary[GmosNorthGrating]
      filter  <- arbitrary[Option[GmosNorthFilter]]
      fpu     <- arbitrary[GmosNorthFpu]
    } yield GmosNorthLongSlit(grating, filter, fpu)
  }

  implicit val arbGmosSouthLongSlit = Arbitrary[GmosSouthLongSlit] {
    for {
      grating <- arbitrary[GmosSouthGrating]
      filter  <- arbitrary[Option[GmosSouthFilter]]
      fpu     <- arbitrary[GmosSouthFpu]
    } yield GmosSouthLongSlit(grating, filter, fpu)
  }

  implicit val arbScienceMode = Arbitrary[ScienceModeBasic] {
    for {
      gmosNLS <- arbitrary[GmosNorthLongSlit]
      gmosSLS <- arbitrary[GmosSouthLongSlit]
      sc      <- Gen.oneOf(gmosNLS, gmosSLS)
    } yield sc
  }

  implicit val cogenGmosNorthLongSlit: Cogen[GmosNorthLongSlit] =
    Cogen[(GmosNorthGrating, Option[GmosNorthFilter], GmosNorthFpu)]
      .contramap(o => (o.grating, o.filter, o.fpu))

  implicit val cogenGmosSouthLongSlit: Cogen[GmosSouthLongSlit] =
    Cogen[(GmosSouthGrating, Option[GmosSouthFilter], GmosSouthFpu)]
      .contramap(o => (o.grating, o.filter, o.fpu))

  implicit val cogenScienceMode: Cogen[ScienceModeBasic] =
    Cogen[Either[GmosNorthLongSlit, GmosSouthLongSlit]]
      .contramap {
        case n: GmosNorthLongSlit => n.asLeft
        case s: GmosSouthLongSlit => s.asRight
      }

}

object ArbScienceMode extends ArbScienceMode
