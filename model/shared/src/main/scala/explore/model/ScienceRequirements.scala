// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Decoder
import io.circe.generic.semiauto.*
import io.circe.refined.given
import lucuma.core.enums.FocalPlane
import lucuma.core.enums.SpectroscopyCapabilities
import lucuma.core.math.Angle
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.odb.json.angle.decoder.given
import lucuma.odb.json.wavelength.decoder.given
import lucuma.schemas.decoders.given
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

sealed trait ScienceRequirements derives Eq

object ScienceRequirements {
  case class Spectroscopy(
    wavelength:         Option[Wavelength],
    resolution:         Option[PosInt],
    signalToNoise:      Option[SignalToNoise],
    signalToNoiseAt:    Option[Wavelength],
    wavelengthCoverage: Option[WavelengthDelta],
    focalPlane:         Option[FocalPlane],
    focalPlaneAngle:    Option[Angle],
    capability:         Option[SpectroscopyCapabilities]
  ) extends ScienceRequirements derives Eq

  object Spectroscopy {
    given Decoder[Spectroscopy] = deriveDecoder

    val wavelength: Lens[Spectroscopy, Option[Wavelength]]               = Focus[Spectroscopy](_.wavelength)
    val resolution: Lens[Spectroscopy, Option[PosInt]]                   = Focus[Spectroscopy](_.resolution)
    val signalToNoise: Lens[Spectroscopy, Option[SignalToNoise]]         =
      Focus[Spectroscopy](_.signalToNoise)
    val signalToNoiseAt: Lens[Spectroscopy, Option[Wavelength]]          =
      Focus[Spectroscopy](_.signalToNoiseAt)
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
}
