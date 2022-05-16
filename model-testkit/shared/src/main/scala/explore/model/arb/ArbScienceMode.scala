// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import cats.syntax.all._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import explore.model.ScienceMode
import lucuma.core.util.arb.ArbGid._
import lucuma.core.util.arb.ArbEnumerated._
import explore.model.ScienceModeBasic
import explore.model.ScienceModeAdvanced

trait ArbScienceMode {
  import ArbScienceModeBasic._
  import ArbScienceModeAdvanced._

  implicit val arbGmosNorthLongSlit = Arbitrary[ScienceMode.GmosNorthLongSlit](
    for {
      basic    <- arbitrary[ScienceModeBasic.GmosNorthLongSlit]
      advanced <- arbitrary[ScienceModeAdvanced.GmosNorthLongSlit]
    } yield ScienceMode.GmosNorthLongSlit(basic, advanced)
  )

  implicit val arbGmosSouthLongSlit = Arbitrary[ScienceMode.GmosSouthLongSlit](
    for {
      basic    <- arbitrary[ScienceModeBasic.GmosSouthLongSlit]
      advanced <- arbitrary[ScienceModeAdvanced.GmosSouthLongSlit]
    } yield ScienceMode.GmosSouthLongSlit(basic, advanced)
  )

  implicit val arbScienceMode = Arbitrary[ScienceMode](
    Gen.oneOf(
      arbitrary[ScienceMode.GmosNorthLongSlit],
      arbitrary[ScienceMode.GmosSouthLongSlit]
    )
  )

  implicit val cogenGmosNorthLongSlit: Cogen[ScienceMode.GmosNorthLongSlit] =
    Cogen[(ScienceModeBasic.GmosNorthLongSlit, ScienceModeAdvanced.GmosNorthLongSlit)]
      .contramap(o => (o.basic, o.advanced))

  implicit val cogenGmosSouthLongSlit: Cogen[ScienceMode.GmosSouthLongSlit] =
    Cogen[(ScienceModeBasic.GmosSouthLongSlit, ScienceModeAdvanced.GmosSouthLongSlit)]
      .contramap(o => (o.basic, o.advanced))

  implicit val cogenScienceMode: Cogen[ScienceMode] =
    Cogen[Either[ScienceMode.GmosNorthLongSlit, ScienceMode.GmosSouthLongSlit]]
      .contramap {
        case n @ ScienceMode.GmosNorthLongSlit(_, _) => n.asLeft
        case s @ ScienceMode.GmosSouthLongSlit(_, _) => s.asRight
      }

}

object ArbScienceMode extends ArbScienceMode
