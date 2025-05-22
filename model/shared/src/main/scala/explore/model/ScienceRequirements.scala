// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.PosInt
import explore.model.enums.ExposureTimeModeType
import explore.model.enums.ExposureTimeModeType.*
import io.circe.Decoder
import io.circe.refined.given
import lucuma.core.enums.FilterType
import lucuma.core.enums.FocalPlane
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
import monocle.Prism
import monocle.macros.GenPrism

object NarrowBand extends NewBoolean
type NarrowBand = NarrowBand.Type
object BroadBand extends NewBoolean
type BroadBand = BroadBand.Type
object Combination extends NewBoolean
type Combination = Combination.Type

sealed trait ScienceRequirements derives Eq:
  val exposureTimeMode: Option[ExposureTimeMode]
  def modeType: Option[ExposureTimeModeType] =
    exposureTimeMode.map(_.modeType)

object ScienceRequirements:
  case class Spectroscopy(
    wavelength:         Option[Wavelength],
    resolution:         Option[PosInt],
    exposureTimeMode:   Option[ExposureTimeMode],
    wavelengthCoverage: Option[WavelengthDelta],
    focalPlane:         Option[FocalPlane],
    focalPlaneAngle:    Option[Angle],
    capability:         Option[SpectroscopyCapabilities]
  ) extends ScienceRequirements derives Eq

  object Spectroscopy:
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

  case class Imaging(
    exposureTimeMode:   Option[ExposureTimeMode],
    minimumFov:         Option[Angle],
    narrowFilters:      NarrowBand = NarrowBand.False,
    broadFilters:       BroadBand = BroadBand.False,
    combinationFilters: Combination = Combination.False
  ) extends ScienceRequirements derives Eq:
    lazy val allowedFilterTypes: Set[FilterType] =
      extension (b: Boolean)
        def toSet(ft: FilterType): Set[FilterType] =
          if b then Set(ft) else Set.empty[FilterType]
      Set.empty[FilterType] ++
        narrowFilters.value.toSet(FilterType.NarrowBand) ++
        broadFilters.toSet(FilterType.BroadBand) ++
        combinationFilters.toSet(FilterType.Combination)

  object Imaging:
    val exposureTimeMode: Lens[Imaging, Option[ExposureTimeMode]] =
      Focus[Imaging](_.exposureTimeMode)
    val signalToNoiseMode: Optional[Imaging, SignalToNoiseMode]   =
      exposureTimeMode.some.andThen(ExposureTimeMode.signalToNoise)
    val timeAndCountMode: Optional[Imaging, TimeAndCountMode]     =
      exposureTimeMode.some.andThen(ExposureTimeMode.timeAndCount)
    val minimumFov: Lens[Imaging, Option[Angle]]                  = Focus[Imaging](_.minimumFov)
    val narrowFilters: Lens[Imaging, NarrowBand]                  = Focus[Imaging](_.narrowFilters)
    val broadFilters: Lens[Imaging, BroadBand]                    = Focus[Imaging](_.broadFilters)
    val combinationFilters: Lens[Imaging, Combination]            = Focus[Imaging](_.combinationFilters)

    val Default: Imaging = Imaging(None, None)

  val spectroscopy: Prism[ScienceRequirements, ScienceRequirements.Spectroscopy] =
    GenPrism[ScienceRequirements, ScienceRequirements.Spectroscopy]

  val imaging: Prism[ScienceRequirements, ScienceRequirements.Imaging] =
    GenPrism[ScienceRequirements, ScienceRequirements.Imaging]

  given Decoder[ScienceRequirements] = Decoder.instance(c => c.get[Spectroscopy]("spectroscopy"))
