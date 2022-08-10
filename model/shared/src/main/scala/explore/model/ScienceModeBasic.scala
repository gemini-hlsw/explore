// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all._
import io.circe.Decoder
import io.circe.generic.semiauto._
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import monocle.Focus
import monocle.Lens

sealed trait ScienceModeBasic extends Product with Serializable derives Eq

object ScienceModeBasic {
  final case class GmosNorthLongSlit(
    grating: GmosNorthGrating,
    filter:  Option[GmosNorthFilter],
    fpu:     GmosNorthFpu
  ) extends ScienceModeBasic
      derives Eq

  object GmosNorthLongSlit {
    given Decoder[GmosNorthLongSlit] = deriveDecoder

    val grating: Lens[GmosNorthLongSlit, GmosNorthGrating]       = Focus[GmosNorthLongSlit](_.grating)
    val filter: Lens[GmosNorthLongSlit, Option[GmosNorthFilter]] =
      Focus[GmosNorthLongSlit](_.filter)
    val fpu: Lens[GmosNorthLongSlit, GmosNorthFpu]               = Focus[GmosNorthLongSlit](_.fpu)
  }

  final case class GmosSouthLongSlit(
    grating: GmosSouthGrating,
    filter:  Option[GmosSouthFilter],
    fpu:     GmosSouthFpu
  ) extends ScienceModeBasic
      derives Eq

  object GmosSouthLongSlit {
    given Decoder[GmosSouthLongSlit] = deriveDecoder

    val grating: Lens[GmosSouthLongSlit, GmosSouthGrating]       = Focus[GmosSouthLongSlit](_.grating)
    val filter: Lens[GmosSouthLongSlit, Option[GmosSouthFilter]] =
      Focus[GmosSouthLongSlit](_.filter)
    val fpu: Lens[GmosSouthLongSlit, GmosSouthFpu]               = Focus[GmosSouthLongSlit](_.fpu)
  }
}
