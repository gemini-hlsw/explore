// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.syntax.all._
import io.circe.Decoder
import lucuma.core.enum._
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

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
    advanced: ScienceModeAdvanced.GmosNorthLongSlit
  ) extends ScienceMode(Instrument.GmosNorth) {
    lazy val grating: GmosNorthGrating = advanced.overrideGrating.getOrElse(basic.grating)

    lazy val filter: Option[GmosNorthFilter] = advanced.overrideFilter.orElse(basic.filter)

    lazy val fpu: GmosNorthFpu = advanced.overrideFpu.getOrElse(basic.fpu)
  }

  object GmosNorthLongSlit {
    implicit val gmosNLongSlitEq: Eq[GmosNorthLongSlit] = Eq.by(x => (x.basic, x.advanced))

    implicit val gmosNLongSlitDecoder: Decoder[GmosNorthLongSlit] = Decoder.instance { c =>
      for {
        basic    <- c.downField("basic").as[ScienceModeBasic.GmosNorthLongSlit]
        advanced <- (c.keys.exists(_.exists(_ === "advanced")) match {
                      case true  =>
                        c.downField("advanced").as[Option[ScienceModeAdvanced.GmosNorthLongSlit]]
                      case false => none.asRight
                    }).map(_.getOrElse(ScienceModeAdvanced.GmosNorthLongSlit.Empty))
      } yield GmosNorthLongSlit(basic, advanced)
    }

    val basic: Lens[GmosNorthLongSlit, ScienceModeBasic.GmosNorthLongSlit] =
      Focus[GmosNorthLongSlit](_.basic)

    val advanced: Lens[GmosNorthLongSlit, ScienceModeAdvanced.GmosNorthLongSlit] =
      Focus[GmosNorthLongSlit](_.advanced)
  }

  final case class GmosSouthLongSlit(
    basic:    ScienceModeBasic.GmosSouthLongSlit,
    advanced: ScienceModeAdvanced.GmosSouthLongSlit
  ) extends ScienceMode(Instrument.GmosSouth) {
    lazy val grating: GmosSouthGrating = advanced.overrideGrating.getOrElse(basic.grating)

    lazy val filter: Option[GmosSouthFilter] = advanced.overrideFilter.orElse(basic.filter)

    lazy val fpu: GmosSouthFpu = advanced.overrideFpu.getOrElse(basic.fpu)
  }

  object GmosSouthLongSlit {
    implicit val gmosSLongSlitEq: Eq[GmosSouthLongSlit] = Eq.by(x => (x.basic, x.advanced))

    implicit val gmosSLongSlitDecoder: Decoder[GmosSouthLongSlit] = Decoder.instance { c =>
      for {
        basic    <- c.downField("basic").as[ScienceModeBasic.GmosSouthLongSlit]
        advanced <- (c.keys.exists(_.exists(_ === "advanced")) match {
                      case true  =>
                        c.downField("advanced").as[Option[ScienceModeAdvanced.GmosSouthLongSlit]]
                      case false => none.asRight
                    }).map(_.getOrElse(ScienceModeAdvanced.GmosSouthLongSlit.Empty))
      } yield GmosSouthLongSlit(basic, advanced)
    }

    val basic: Lens[GmosSouthLongSlit, ScienceModeBasic.GmosSouthLongSlit] =
      Focus[GmosSouthLongSlit](_.basic)

    val advanced: Lens[GmosSouthLongSlit, ScienceModeAdvanced.GmosSouthLongSlit] =
      Focus[GmosSouthLongSlit](_.advanced)

  }

  val gmosNorthLongSlit: Prism[ScienceMode, GmosNorthLongSlit] =
    GenPrism[ScienceMode, GmosNorthLongSlit]

  val gmosSouthLongSlit: Prism[ScienceMode, GmosSouthLongSlit] =
    GenPrism[ScienceMode, GmosSouthLongSlit]
}
