// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import cats.syntax.all._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import explore.model.ScienceModeBasic
import lucuma.core.util.arb.ArbGid._
import lucuma.core.util.arb.ArbEnumerated._
import lucuma.core.enum.GmosNorthFilter
import lucuma.core.enum.GmosNorthGrating
import lucuma.core.enum.GmosNorthFpu
import lucuma.core.enum.GmosSouthFilter
import lucuma.core.enum.GmosSouthGrating
import lucuma.core.enum.GmosSouthFpu

trait ArbScienceModeBasic {

  implicit val arbGmosNorthLongSlitBasic = Arbitrary[ScienceModeBasic.GmosNorthLongSlit](
    for {
      grating <- arbitrary[GmosNorthGrating]
      filter  <- arbitrary[Option[GmosNorthFilter]]
      fpu     <- arbitrary[GmosNorthFpu]
    } yield ScienceModeBasic.GmosNorthLongSlit(grating, filter, fpu)
  )

  implicit val arbGmosSouthLongSlitBasic = Arbitrary[ScienceModeBasic.GmosSouthLongSlit](
    for {
      grating <- arbitrary[GmosSouthGrating]
      filter  <- arbitrary[Option[GmosSouthFilter]]
      fpu     <- arbitrary[GmosSouthFpu]
    } yield ScienceModeBasic.GmosSouthLongSlit(grating, filter, fpu)
  )

  implicit val arbScienceModeBasic = Arbitrary[ScienceModeBasic](
    Gen.oneOf(
      arbitrary[ScienceModeBasic.GmosNorthLongSlit],
      arbitrary[ScienceModeBasic.GmosSouthLongSlit]
    )
  )

  implicit val cogenGmosNorthLongSlitBasic: Cogen[ScienceModeBasic.GmosNorthLongSlit] =
    Cogen[(GmosNorthGrating, Option[GmosNorthFilter], GmosNorthFpu)]
      .contramap(o => (o.grating, o.filter, o.fpu))

  implicit val cogenGmosSouthLongSlitBasic: Cogen[ScienceModeBasic.GmosSouthLongSlit] =
    Cogen[(GmosSouthGrating, Option[GmosSouthFilter], GmosSouthFpu)]
      .contramap(o => (o.grating, o.filter, o.fpu))

  implicit val cogenScienceModeBasic: Cogen[ScienceModeBasic] =
    Cogen[Either[ScienceModeBasic.GmosNorthLongSlit, ScienceModeBasic.GmosSouthLongSlit]]
      .contramap {
        case n @ ScienceModeBasic.GmosNorthLongSlit(_, _, _) => n.asLeft
        case s @ ScienceModeBasic.GmosSouthLongSlit(_, _, _) => s.asRight
      }

}

object ArbScienceModeBasic extends ArbScienceModeBasic
