// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import explore.model.ExploreModelValidators
import explore.model.enums.WavelengthUnits
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.validation.*
import lucuma.refined.*
import lucuma.ui.input.ChangeAuditor

trait ConfigurationFormats:
  private lazy val slitLengthBaseAuditor = ChangeAuditor
    .fromInputValidWedge(ExploreModelValidators.decimalArcsecondsValidWedge)
    .allow(s => s === "0" || s === "0.")
  lazy val slitLengthChangeAuditor       = slitLengthBaseAuditor
    .decimal(2.refined)
    .optional
  lazy val slitLengthFormat              = ExploreModelValidators.decimalArcsecondsValidWedge.optional
  lazy val wvMicroInput                  = ExploreModelValidators.wavelengthMicroValidWedge.optional
  lazy val wvNanoInput                   = ExploreModelValidators.wavelengthNanoValidWedge.optional
  lazy val wvDeltaMicroInput             = ExploreModelValidators.wavelengthMicroDeltaValidWedge.optional
  lazy val wvDeltaNanoInput              = ExploreModelValidators.wavelengthNanoDeltaValidWedge.optional
  extension (u: WavelengthUnits)
    def toAuditor: ChangeAuditor =
      u match
        case WavelengthUnits.Micrometers => ChangeAuditor.posBigDecimal(3.refined)
        case WavelengthUnits.Nanometers  => ChangeAuditor.posBigDecimal(1.refined)

    def toSNAuditor: ChangeAuditor = toAuditor

    def toInputWedge: InputValidWedge[Option[Wavelength]] =
      u match
        case WavelengthUnits.Micrometers => wvMicroInput
        case WavelengthUnits.Nanometers  => wvNanoInput

    def toInputFormat: InputValidFormat[Wavelength] =
      u match
        case WavelengthUnits.Micrometers => ExploreModelValidators.wavelengthMicroValidWedge
        case WavelengthUnits.Nanometers  => ExploreModelValidators.wavelengthNanoValidWedge

    def toDeltaInputWedge: InputValidWedge[Option[WavelengthDelta]] =
      u match
        case WavelengthUnits.Micrometers => wvDeltaMicroInput
        case WavelengthUnits.Nanometers  => wvDeltaNanoInput

object ConfigurationFormats extends ConfigurationFormats
