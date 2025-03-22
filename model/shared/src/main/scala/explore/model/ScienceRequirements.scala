// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import explore.model.enums.ExposureTimeModeType
import io.circe.Decoder
import io.circe.refined.given
import lucuma.core.enums.FocalPlane
import lucuma.core.enums.SpectroscopyCapabilities
import lucuma.core.math.Angle
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.ExposureTimeMode.SignalToNoiseMode
import lucuma.core.util.TimeSpan
import lucuma.odb.json.angle.decoder.given
import lucuma.odb.json.wavelength.decoder.given
import lucuma.schemas.decoders.given
import monocle.Focus
import monocle.Lens
import monocle.Optional
import monocle.Prism
import monocle.macros.GenPrism
import monocle.std.either.*

sealed trait ScienceRequirements derives Eq

object ScienceRequirements:
  case class TimeAndCountModeInfo(
    time:  Option[TimeSpan],
    count: Option[NonNegInt],
    at:    Option[Wavelength]
  ) derives Eq

  object TimeAndCountModeInfo:
    val time: Lens[TimeAndCountModeInfo, Option[TimeSpan]] =
      Focus[TimeAndCountModeInfo](_.time)

    val count: Lens[TimeAndCountModeInfo, Option[NonNegInt]] =
      Focus[TimeAndCountModeInfo](_.count)

    val at: Lens[TimeAndCountModeInfo, Option[Wavelength]] =
      Focus[TimeAndCountModeInfo](_.at)

    val Default = TimeAndCountModeInfo(None, None, None)

  case class SignalToNoiseModeInfo(value: Option[SignalToNoise], at: Option[Wavelength]) derives Eq

  object SignalToNoiseModeInfo:
    val value: Lens[SignalToNoiseModeInfo, Option[SignalToNoise]] =
      Focus[SignalToNoiseModeInfo](_.value)

    val at: Lens[SignalToNoiseModeInfo, Option[Wavelength]] =
      Focus[SignalToNoiseModeInfo](_.at)

    val Default = SignalToNoiseModeInfo(None, None)

  case class ExposureTimeModeInfo(mode: Either[SignalToNoiseModeInfo, TimeAndCountModeInfo])
      derives Eq:
    val exposureMode: ExposureTimeModeType = mode match
      case Left(_)  => ExposureTimeModeType.SignalToNoise
      case Right(_) => ExposureTimeModeType.TimeAndCount

    def asSignalToNoiseMode: ExposureTimeModeInfo = this match
      case s @ ExposureTimeModeInfo(Left(_))                           =>
        s
      case ExposureTimeModeInfo(Right(TimeAndCountModeInfo(_, _, at))) =>
        ExposureTimeModeInfo(Left(SignalToNoiseModeInfo(None, at)))

    def asTimeAndCountMode: ExposureTimeModeInfo = this match
      case s @ ExposureTimeModeInfo(Right(_))                       =>
        s
      case ExposureTimeModeInfo(Left(SignalToNoiseModeInfo(_, at))) =>
        ExposureTimeModeInfo(Right(TimeAndCountModeInfo(None, None, at)))

    def at: Option[Wavelength] = mode match
      case Left(SignalToNoiseModeInfo(_, at))    => at
      case Right(TimeAndCountModeInfo(_, _, at)) => at

    def withSNAt(w: Option[Wavelength]): ExposureTimeModeInfo =
      mode match
        case Left(SignalToNoiseModeInfo(v, _))    =>
          ExposureTimeModeInfo(Left(SignalToNoiseModeInfo(v, w)))
        case Right(TimeAndCountModeInfo(t, c, _)) =>
          ExposureTimeModeInfo(Right(TimeAndCountModeInfo(t, c, w)))

  object ExposureTimeModeInfo {
    val mode: Lens[ExposureTimeModeInfo, Either[SignalToNoiseModeInfo, TimeAndCountModeInfo]] =
      Focus[ExposureTimeModeInfo](_.mode)

    val signalToNoise: Optional[ExposureTimeModeInfo, SignalToNoiseModeInfo] =
      mode.andThen(stdLeft.asOptional)

    val timeAndCount: Optional[ExposureTimeModeInfo, TimeAndCountModeInfo] =
      mode.andThen(stdRight.asOptional)

    val at: Optional[ExposureTimeModeInfo, Option[Wavelength]] =
      signalToNoise
        .andThen(SignalToNoiseModeInfo.at)
        .orElse(timeAndCount.andThen(TimeAndCountModeInfo.at))

    val Default = ExposureTimeModeInfo(Left(SignalToNoiseModeInfo.Default))

    def fromOption(e: Option[ExposureTimeMode]) = e match {
      case Some(ExposureTimeMode.SignalToNoiseMode(v, a))   =>
        ExposureTimeModeInfo(Left(SignalToNoiseModeInfo(v.some, a.some)))
      case Some(ExposureTimeMode.TimeAndCountMode(t, c, a)) =>
        ExposureTimeModeInfo(Right(TimeAndCountModeInfo(t.some, c.some, a.some)))
      case None                                             => ExposureTimeModeInfo.Default
    }

  }

  case class Spectroscopy(
    wavelength:         Option[Wavelength],
    resolution:         Option[PosInt],
    exposureTimeMode:   ExposureTimeModeInfo,
    wavelengthCoverage: Option[WavelengthDelta],
    focalPlane:         Option[FocalPlane],
    focalPlaneAngle:    Option[Angle],
    capability:         Option[SpectroscopyCapabilities]
  ) extends ScienceRequirements derives Eq {
    def withSNAt(w: Option[Wavelength]): Spectroscopy =
      copy(exposureTimeMode = exposureTimeMode.withSNAt(w))

    def extractSignalToNoise: (Option[SignalToNoise], Option[Wavelength]) =
      exposureTimeMode.mode match
        case Left(SignalToNoiseModeInfo(v @ Some(_), a @ Some(_))) =>
          (v, a)
        case _                                                     => (none, none)

    def exposureTimeModeOption: Option[ExposureTimeMode] = exposureTimeMode.mode match
      case Left(SignalToNoiseModeInfo(Some(sn), Some(wl)))         =>
        Some(ExposureTimeMode.SignalToNoiseMode(sn, wl))
      case Right(TimeAndCountModeInfo(Some(t), Some(c), Some(wl))) =>
        Some(ExposureTimeMode.TimeAndCountMode(t, c, wl))
      case _                                                       => None
  }

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
      } yield
        val etmi = ExposureTimeModeInfo.fromOption(etm)
        Spectroscopy(wl, res, etmi, cov, fp, fpa, cap)

    val wavelength: Lens[Spectroscopy, Option[Wavelength]]               = Focus[Spectroscopy](_.wavelength)
    val resolution: Lens[Spectroscopy, Option[PosInt]]                   = Focus[Spectroscopy](_.resolution)
    val exposureTimeMode: Lens[Spectroscopy, ExposureTimeModeInfo]       =
      Focus[Spectroscopy](_.exposureTimeMode)
    val signalToNoiseMode: Optional[Spectroscopy, SignalToNoiseModeInfo] =
      exposureTimeMode.andThen(ExposureTimeModeInfo.signalToNoise)
    val timeAndCountMode: Optional[Spectroscopy, TimeAndCountModeInfo]   =
      exposureTimeMode.andThen(ExposureTimeModeInfo.timeAndCount)
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
