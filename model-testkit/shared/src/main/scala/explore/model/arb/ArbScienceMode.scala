// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import cats.syntax.all.*
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import explore.model.ScienceMode
import lucuma.core.util.arb.ArbGid.*
import lucuma.core.util.arb.ArbEnumerated.*
import explore.model.ScienceModeBasic
import explore.model.ScienceModeAdvanced

trait ArbScienceMode {
  import ArbScienceModeBasic.given
  import ArbScienceModeAdvanced.given

  given Arbitrary[ScienceMode.GmosNorthLongSlit] =
    Arbitrary[ScienceMode.GmosNorthLongSlit](
      for {
        basic    <- arbitrary[ScienceModeBasic.GmosNorthLongSlit]
        advanced <- arbitrary[ScienceModeAdvanced.GmosNorthLongSlit]
      } yield ScienceMode.GmosNorthLongSlit(basic, advanced)
    )

  given Arbitrary[ScienceMode.GmosSouthLongSlit] =
    Arbitrary[ScienceMode.GmosSouthLongSlit](
      for {
        basic    <- arbitrary[ScienceModeBasic.GmosSouthLongSlit]
        advanced <- arbitrary[ScienceModeAdvanced.GmosSouthLongSlit]
      } yield ScienceMode.GmosSouthLongSlit(basic, advanced)
    )

  given Arbitrary[ScienceMode] = Arbitrary[ScienceMode](
    Gen.oneOf(
      arbitrary[ScienceMode.GmosNorthLongSlit],
      arbitrary[ScienceMode.GmosSouthLongSlit]
    )
  )

  given Cogen[ScienceMode.GmosNorthLongSlit] =
    Cogen[(ScienceModeBasic.GmosNorthLongSlit, ScienceModeAdvanced.GmosNorthLongSlit)]
      .contramap(o => (o.basic, o.advanced))

  given Cogen[ScienceMode.GmosSouthLongSlit] =
    Cogen[(ScienceModeBasic.GmosSouthLongSlit, ScienceModeAdvanced.GmosSouthLongSlit)]
      .contramap(o => (o.basic, o.advanced))

  given Cogen[ScienceMode] =
    Cogen[Either[ScienceMode.GmosNorthLongSlit, ScienceMode.GmosSouthLongSlit]]
      .contramap {
        case n @ ScienceMode.GmosNorthLongSlit(_, _) => n.asLeft
        case s @ ScienceMode.GmosSouthLongSlit(_, _) => s.asRight
      }

}

object ArbScienceMode extends ArbScienceMode
