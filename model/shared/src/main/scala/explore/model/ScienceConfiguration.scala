// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.syntax.all._
import io.circe.Decoder
import io.circe._
import io.circe.generic.semiauto._
import lucuma.core.enum.GmosNorthDisperser
import lucuma.core.enum.GmosNorthFilter
import lucuma.core.enum.GmosSouthDisperser
import lucuma.core.enum.GmosSouthFilter
import lucuma.core.math.Angle
import lucuma.schemas.decoders._

sealed trait ScienceConfiguration extends Product with Serializable

object ScienceConfiguration {
  implicit val scienceConfigurationEq: Eq[ScienceConfiguration] =
    Eq.instance {
      case (a: GmosSouthLongSlit, b: GmosSouthLongSlit) => a === b
      case (a: GmosNorthLongSlit, b: GmosNorthLongSlit) => a === b
      case _                                            => false
    }

  implicit val scienceConfigurationDecoder: Decoder[ScienceConfiguration] =
    new Decoder[ScienceConfiguration] {
      final def apply(c: HCursor): Decoder.Result[ScienceConfiguration] =
        c.downField("gmosNorthLongSlit")
          .as[GmosNorthLongSlit]
          .orElse(c.downField("gmosSouthLongSlit").as[GmosSouthLongSlit])
    }
}

final case class GmosNorthLongSlit(
  filter:    Option[GmosNorthFilter],
  disperser: GmosNorthDisperser,
  slitWidth: Angle
) extends ScienceConfiguration

object GmosNorthLongSlit {
  implicit val gmosNLongSlitEq: Eq[GmosNorthLongSlit] =
    Eq.by(x => (x.filter, x.disperser, x.slitWidth))

  implicit val gmosNDecoder: Decoder[GmosNorthLongSlit] = deriveDecoder
}

final case class GmosSouthLongSlit(
  filter:    Option[GmosSouthFilter],
  disperser: GmosSouthDisperser,
  slitWidth: Angle
) extends ScienceConfiguration

object GmosSouthLongSlit {
  implicit val gmosSLongSlitEq: Eq[GmosSouthLongSlit] =
    Eq.by(x => (x.filter, x.disperser, x.slitWidth))

  implicit val gmosSDecoder: Decoder[GmosSouthLongSlit] = deriveDecoder
}
