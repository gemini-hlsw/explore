// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import explore.model.itc.CoverageCenterWavelength
import io.circe.Decoder
import io.circe.generic.semiauto.*
import io.circe.refined.*
import lucuma.core.enums.*
import lucuma.core.math.Wavelength
import lucuma.schemas.decoders.given

// For when we don't need the whole observing mode, such as in the ObsSummary.
// This is also used to create the configuration.
sealed abstract class BasicConfiguration(val instrument: Instrument)
    extends Product
    with Serializable derives Eq {

  def fpuAlternative: Option[Either[GmosNorthFpu, GmosSouthFpu]] = this match {
    case n: BasicConfiguration.GmosNorthLongSlit => n.fpu.asLeft.some
    case s: BasicConfiguration.GmosSouthLongSlit => s.fpu.asRight.some
  }

  def siteFor: Site = this match {
    case n: BasicConfiguration.GmosNorthLongSlit => Site.GN
    case s: BasicConfiguration.GmosSouthLongSlit => Site.GS
  }
}

object BasicConfiguration:
  given Decoder[BasicConfiguration] =
    Decoder
      .instance(c =>
        c.downField("gmosNorthLongSlit")
          .as[GmosNorthLongSlit]
          .orElse(
            c.downField("gmosSouthLongSlit").as[GmosSouthLongSlit]
          )
      )

  case class GmosNorthLongSlit(
    grating:           GmosNorthGrating,
    filter:            Option[GmosNorthFilter],
    fpu:               GmosNorthFpu,
    centralWavelength: CoverageCenterWavelength
  ) extends BasicConfiguration(Instrument.GmosNorth)
      derives Eq

  object GmosNorthLongSlit:
    given Decoder[GmosNorthLongSlit] = deriveDecoder

  case class GmosSouthLongSlit(
    grating:           GmosSouthGrating,
    filter:            Option[GmosSouthFilter],
    fpu:               GmosSouthFpu,
    centralWavelength: CoverageCenterWavelength
  ) extends BasicConfiguration(Instrument.GmosSouth)
      derives Eq

  object GmosSouthLongSlit:
    given Decoder[GmosSouthLongSlit] = deriveDecoder
