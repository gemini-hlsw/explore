// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.PosInt
import explore.model.enums.ExposureTimeModeType
import explore.model.enums.ExposureTimeModeType.*
import io.circe.Decoder
import io.circe.refined.given
import lucuma.core.enums.FocalPlane
import lucuma.core.enums.SpectroscopyCapabilities
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.ExposureTimeMode.SignalToNoiseMode
import lucuma.core.model.ExposureTimeMode.TimeAndCountMode
import lucuma.odb.json.angle.decoder.given
import lucuma.odb.json.wavelength.decoder.given
import lucuma.schemas.decoders.given
import monocle.Focus
import monocle.Lens
import monocle.Optional
import monocle.Prism
import monocle.macros.GenPrism

sealed trait ScienceRequirements derives Eq

object ScienceRequirements:
  case class Spectroscopy(
    wavelength:         Option[Wavelength],
    resolution:         Option[PosInt],
    exposureTimeMode:   Option[ExposureTimeMode],
    wavelengthCoverage: Option[WavelengthDelta],
    focalPlane:         Option[FocalPlane],
    focalPlaneAngle:    Option[Angle],
    capability:         Option[SpectroscopyCapabilities]
  ) extends ScienceRequirements derives Eq:
    def modeType: Option[ExposureTimeModeType] =
      exposureTimeMode.map(_.modeType)

  object Spectroscopy {
    given Decoder[Spectroscopy] = Decoder.instance: c =>
      for {
        wl  <- c.downField("wavelength").as[Option[Wavelength]]
        res <- c.downField("resolution").as[Option[PosInt]]
        etm <- c.downField("exposureTimeMode").as[Option[ExposureTimeMode]]
        cov <- c.downField("wavelengthCoverage").as[Option[WavelengthDelta]]
        fp  <- c.downField("focalPlane").as[Option[FocalPlane]]
        fpa <- c.downField("focalPlaneAngle").as[Option[Angle]]
        cap <- c.downField("capability").as[Option[SpectroscopyCapabilities]]
      } yield Spectroscopy(wl, res, etm, cov, fp, fpa, cap)

    val wavelength: Lens[Spectroscopy, Option[Wavelength]]               = Focus[Spectroscopy](_.wavelength)
    val resolution: Lens[Spectroscopy, Option[PosInt]]                   = Focus[Spectroscopy](_.resolution)
    val exposureTimeMode: Lens[Spectroscopy, Option[ExposureTimeMode]]   =
      Focus[Spectroscopy](_.exposureTimeMode)
    val signalToNoiseMode: Optional[Spectroscopy, SignalToNoiseMode]     =
      exposureTimeMode.some.andThen(ExposureTimeMode.signalToNoise)
    val timeAndCountMode: Optional[Spectroscopy, TimeAndCountMode]       =
      exposureTimeMode.some.andThen(ExposureTimeMode.timeAndCount)
    val wavelengthCoverage: Lens[Spectroscopy, Option[WavelengthDelta]]  =
      Focus[Spectroscopy](_.wavelengthCoverage)
    val focalPlane: Lens[Spectroscopy, Option[FocalPlane]]               = Focus[Spectroscopy](_.focalPlane)
    val focalPlaneAngle: Lens[Spectroscopy, Option[Angle]]               = Focus[Spectroscopy](_.focalPlaneAngle)
    val capability: Lens[Spectroscopy, Option[SpectroscopyCapabilities]] =
      Focus[Spectroscopy](_.capability)

  }

  val spectroscopy: Prism[ScienceRequirements, ScienceRequirements.Spectroscopy] =
    GenPrism[ScienceRequirements, ScienceRequirements.Spectroscopy]

  given Decoder[ScienceRequirements] = Decoder.instance(c => c.get[Spectroscopy]("spectroscopy"))
