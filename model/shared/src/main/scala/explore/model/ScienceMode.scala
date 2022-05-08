// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.syntax.all._
import io.circe.Decoder
import io.circe.generic.semiauto._
import lucuma.core.enum._

sealed abstract class ScienceMode(val instrument: Instrument) extends Product with Serializable

object ScienceMode {
  implicit val scienceModeEq: Eq[ScienceMode] = Eq.instance {
    case (a: GmosNorthLongSlit, b: GmosNorthLongSlit) => a === b
    case (a: GmosSouthLongSlit, b: GmosSouthLongSlit) => a === b
    case _                                            => false
  }

  implicit val scienceModeDecoder: Decoder[ScienceMode] =
    Decoder
      .instance(c =>
        c.downField("gmosNorthLongSlit")
          .as[GmosNorthLongSlit]
          .orElse(
            c.downField("gmosSouthLongSlit").as[GmosSouthLongSlit]
          )
      )

  final case class GmosNorthLongSlit(
    basic:    ScienceModeBasic.GmosNorthLongSlit,
    advanced: Option[ScienceModeAdvanced.GmosNorthLongSlit]
  ) extends ScienceMode(Instrument.GmosNorth) {
    lazy val grating: GmosNorthGrating =
      advanced.flatMap(_.overrideGrating).getOrElse(basic.grating)

    lazy val filter: Option[GmosNorthFilter] =
      advanced.flatMap(_.overrideFilter).orElse(basic.filter)

    lazy val fpu: GmosNorthFpu = advanced.flatMap(_.overrideFpu).getOrElse(basic.fpu)
  }

  object GmosNorthLongSlit {
    implicit val gmosNLongSlitEq: Eq[GmosNorthLongSlit] = Eq.by(x => (x.basic, x.advanced))

    implicit val gmosNLongSlitDecoder: Decoder[GmosNorthLongSlit] = deriveDecoder
  }

  final case class GmosSouthLongSlit(
    basic:    ScienceModeBasic.GmosSouthLongSlit,
    advanced: Option[ScienceModeAdvanced.GmosSouthLongSlit]
  ) extends ScienceMode(Instrument.GmosSouth) {
    lazy val grating: GmosSouthGrating =
      advanced.flatMap(_.overrideGrating).getOrElse(basic.grating)

    lazy val filter: Option[GmosSouthFilter] =
      advanced.flatMap(_.overrideFilter).orElse(basic.filter)

    lazy val fpu: GmosSouthFpu = advanced.flatMap(_.overrideFpu).getOrElse(basic.fpu)
  }

  object GmosSouthLongSlit {
    implicit val gmosSLongSlitEq: Eq[GmosSouthLongSlit] = Eq.by(x => (x.basic, x.advanced))

    implicit val gmosSLongSlitDecoder: Decoder[GmosSouthLongSlit] = deriveDecoder
  }
}
