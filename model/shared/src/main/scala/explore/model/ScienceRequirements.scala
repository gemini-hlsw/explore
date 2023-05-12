// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.FocalPlane
import lucuma.core.enums.SpectroscopyCapabilities
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.math.Angle
import io.circe.generic.semiauto.*
import io.circe.Decoder
import cats.syntax.all.*
import lucuma.schemas.decoders.given
import io.circe.refined.given
import eu.timepit.refined.cats.given

sealed trait ScienceRequirements derives Eq

object ScienceRequirements:

  case class Spectroscopy(
    wavelength:         Option[Wavelength],
    resolution:         Option[PosInt],
    signalToNoise:      Option[SignalToNoise],
    signalToNoiseAt:    Option[Wavelength],
    wavelengthCoverage: Option[WavelengthDelta],
    focalPlane:         Option[FocalPlane],
    focalPlaneAngle:    Option[Angle],
    capability:         Option[SpectroscopyCapabilities]
  ) extends ScienceRequirements
      derives Eq

  object Spectroscopy:
    given Decoder[Spectroscopy] = deriveDecoder
    // TODO LENSES

  given Decoder[ScienceRequirements] =
    Decoder[Spectroscopy].widen
