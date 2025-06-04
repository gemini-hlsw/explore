// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.core.enums.FilterType
import lucuma.core.enums.FocalPlane
import lucuma.core.enums.ScienceMode
import lucuma.core.enums.SpectroscopyCapabilities
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.ExposureTimeMode.SignalToNoiseMode
import lucuma.core.model.ExposureTimeMode.TimeAndCountMode
import lucuma.core.util.NewBoolean
import lucuma.odb.json.angle.decoder.given
import lucuma.odb.json.wavelength.decoder.given
import lucuma.schemas.decoders.given
import monocle.Focus
import monocle.Lens
import monocle.Optional
import monocle.std.either.*

object NarrowBand extends NewBoolean
type NarrowBand = NarrowBand.Type
object BroadBand extends NewBoolean
type BroadBand = BroadBand.Type
object Combination extends NewBoolean
type Combination = Combination.Type

case class ScienceRequirements(
  exposureTimeMode: Option[ExposureTimeMode],
  scienceMode:      Option[Either[ScienceRequirements.Spectroscopy, ScienceRequirements.Imaging]]
) derives Eq:
  lazy val exposureTimeModeType: Option[ExposureTimeModeType] =
    exposureTimeMode.map(_.modeType)

  lazy val scienceModeType: Option[ScienceMode] =
    scienceMode.map(_.fold(_ => ScienceMode.Spectroscopy, _ => ScienceMode.Imaging))

object ScienceRequirements:
  case class Spectroscopy(
    wavelength:         Option[Wavelength],
    resolution:         Option[PosInt],
    wavelengthCoverage: Option[WavelengthDelta],
    focalPlane:         Option[FocalPlane],
    focalPlaneAngle:    Option[Angle],
    capability:         Option[SpectroscopyCapabilities]
  ) derives Eq

  object Spectroscopy:
    val Default: Spectroscopy = Spectroscopy(None, None, None, None, None, None)

    given Decoder[Spectroscopy] = Decoder.instance: c =>
      for {
        wl  <- c.downField("wavelength").as[Option[Wavelength]]
        res <- c.downField("resolution").as[Option[PosInt]]
        cov <- c.downField("wavelengthCoverage").as[Option[WavelengthDelta]]
        fp  <- c.downField("focalPlane").as[Option[FocalPlane]]
        fpa <- c.downField("focalPlaneAngle").as[Option[Angle]]
        cap <- c.downField("capability").as[Option[SpectroscopyCapabilities]]
      } yield Spectroscopy(wl, res, cov, fp, fpa, cap)

    val wavelength: Lens[Spectroscopy, Option[Wavelength]]               = Focus[Spectroscopy](_.wavelength)
    val resolution: Lens[Spectroscopy, Option[PosInt]]                   = Focus[Spectroscopy](_.resolution)
    val wavelengthCoverage: Lens[Spectroscopy, Option[WavelengthDelta]]  =
      Focus[Spectroscopy](_.wavelengthCoverage)
    val focalPlane: Lens[Spectroscopy, Option[FocalPlane]]               = Focus[Spectroscopy](_.focalPlane)
    val focalPlaneAngle: Lens[Spectroscopy, Option[Angle]]               = Focus[Spectroscopy](_.focalPlaneAngle)
    val capability: Lens[Spectroscopy, Option[SpectroscopyCapabilities]] =
      Focus[Spectroscopy](_.capability)

  case class Imaging(
    minimumFov:      Option[Angle],
    narrowFilters:   Option[NarrowBand],
    broadFilters:    Option[BroadBand],
    combinedFilters: Option[Combination]
  ) derives Eq:
    lazy val allowedFilterTypes: Set[FilterType] =
      extension (b: Boolean)
        def toSet(ft: FilterType): Set[FilterType] =
          if b then Set(ft) else Set.empty[FilterType]
      Set.empty[FilterType] ++
        narrowFilters.foldMap(_.toSet(FilterType.NarrowBand)) ++
        broadFilters.foldMap(_.toSet(FilterType.BroadBand)) ++
        combinedFilters.foldMap(_.toSet(FilterType.Combination))

  object Imaging:
    val Default: Imaging =
      // By setting the booleans, we prevent confusion with an empty spectroscopy when
      // getting a response from the odb.
      Imaging(None, NarrowBand.False.some, BroadBand.False.some, Combination.False.some)

    given Decoder[Imaging] = Decoder.instance: c =>
      for {
        fov <- c.downField("minimumFov").as[Option[Angle]]
        nf  <- c.downField("narrowFilters").as[Option[NarrowBand]]
        bf  <- c.downField("broadFilters").as[Option[BroadBand]]
        cf  <- c.downField("combinedFilters").as[Option[Combination]]
      } yield Imaging(fov, nf, bf, cf)

    val minimumFov: Lens[Imaging, Option[Angle]]            = Focus[Imaging](_.minimumFov)
    val narrowFilters: Lens[Imaging, Option[NarrowBand]]    = Focus[Imaging](_.narrowFilters)
    val broadFilters: Lens[Imaging, Option[BroadBand]]      = Focus[Imaging](_.broadFilters)
    val combinedFilters: Lens[Imaging, Option[Combination]] = Focus[Imaging](_.combinedFilters)

  val exposureTimeMode: Lens[ScienceRequirements, Option[ExposureTimeMode]] =
    Focus[ScienceRequirements](_.exposureTimeMode)
  val signalToNoiseMode: Optional[ScienceRequirements, SignalToNoiseMode]   =
    exposureTimeMode.some.andThen(ExposureTimeMode.signalToNoise)
  val timeAndCountMode: Optional[ScienceRequirements, TimeAndCountMode]     =
    exposureTimeMode.some.andThen(ExposureTimeMode.timeAndCount)

  val scienceMode: Lens[ScienceRequirements, Option[Either[Spectroscopy, Imaging]]] =
    Focus[ScienceRequirements](_.scienceMode)

  val spectroscopy: Optional[ScienceRequirements, Spectroscopy] =
    scienceMode.some.andThen(stdLeft)

  val imaging: Optional[ScienceRequirements, Imaging] =
    scienceMode.some.andThen(stdRight)

  given Decoder[ScienceRequirements] = Decoder.instance: c =>
    for {
      spec <- c.get[Option[Spectroscopy]]("spectroscopy")
      img  <- c.get[Option[Imaging]]("imaging")
      etm  <- c.get[Option[ExposureTimeMode]]("exposureTimeMode")
      mode  = (spec, img) match
                case (Some(s), None) => Some(Left(s))
                case (None, Some(i)) => Some(Right(i))
                // Default to spectroscopy
                case _               => Some(Left(Spectroscopy.Default))
    } yield ScienceRequirements(etm, mode)
