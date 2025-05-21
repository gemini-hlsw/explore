// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.Eq
import cats.derived.*
import cats.implicits.*
import explore.model.SupportedInstruments
import io.circe.Decoder
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.odb.json.angle.decoder.given
import monocle.Getter
import monocle.Lens
import monocle.macros.GenLens

case class ImagingModeRow(
  id:         Option[Int], // we number the modes for the UI
  instrument: ItcInstrumentConfig,
  ao:         ModeAO,
  fov:        Angle
) extends ModeRow:
  val enabled = SupportedInstruments.contains_(instrument.instrument)

object ImagingModeRow {

  val id: Lens[ImagingModeRow, Option[Int]] = GenLens[ImagingModeRow](_.id)

  val instrumentConfig: Lens[ImagingModeRow, ItcInstrumentConfig] =
    GenLens[ImagingModeRow](_.instrument)

  val instrument: Getter[ImagingModeRow, Instrument] =
    instrumentConfig.andThen(ItcInstrumentConfig.instrument)

  val ao: Lens[ImagingModeRow, ModeAO] = GenLens[ImagingModeRow](_.ao)

  val fov: Lens[ImagingModeRow, Angle] = GenLens[ImagingModeRow](_.fov)

  def filter: Getter[ImagingModeRow, ItcInstrumentConfig#Filter] =
    instrumentConfig.andThen(ItcInstrumentConfig.filter)

  // decoders for instruments are used locally as they are not lawful
  private given Decoder[ItcInstrumentConfig.GmosNorthImaging] =
    _.downField("filter")
      .as[GmosNorthFilter]
      .map(filter => ItcInstrumentConfig.GmosNorthImaging(filter, none))

  private given Decoder[ItcInstrumentConfig.GmosSouthImaging] =
    _.downField("filter")
      .as[GmosSouthFilter]
      .map(filter => ItcInstrumentConfig.GmosSouthImaging(filter, none))

  given Decoder[ImagingModeRow] = c =>
    for {
      ao        <- c.downField("adaptiveOptics").as[Boolean]
      fov       <- c.downField("fov").as[Angle]
      gmosNorth <- c.downField("gmosNorth").as[Option[ItcInstrumentConfig.GmosNorthImaging]]
      gmosSouth <- c.downField("gmosSouth").as[Option[ItcInstrumentConfig.GmosSouthImaging]]
    } yield gmosNorth
      .orElse(gmosSouth)
      .map { i =>
        ImagingModeRow(
          none,
          i,
          ModeAO(ao),
          fov
        )
      }
      .getOrElse(sys.error("Instrument not found"))
}

case class ImagingModesMatrix(matrix: List[ImagingModeRow]) derives Eq

object ImagingModesMatrix {
  val empty: ImagingModesMatrix = ImagingModesMatrix(Nil)
}
