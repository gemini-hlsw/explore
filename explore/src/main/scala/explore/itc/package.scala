// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import explore.Icons
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Band
import lucuma.core.enums.CalibrationRole
import lucuma.core.math.BrightnessValue
import lucuma.core.math.LineFluxValue
import lucuma.core.math.LineWidthValue
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.Units
import lucuma.core.syntax.display.*
import lucuma.react.fa.*
import lucuma.react.floatingui.syntax.*
import lucuma.ui.syntax.all.given

extension (role: Option[CalibrationRole])
  // Icon to indicate a field is required to do ITC calculations
  def renderRequiredForITCIcon: TagMod =
    <.span(
      LayeredIcon(fixedWidth = true, clazz = ExploreStyles.WarningIcon)(
        Icons.StarExclamation.withSize(IconSize.X1),
        TextLayer("ITC", clazz = ExploreStyles.RequiredForItcText, inverse = true)
      )
    ).withTooltip("Required for ITC")

protected enum BrightnessValues:
  case ForBand(band: Band, value: BrightnessValue, units: Units)
  case ForEmissionLine(
    wavelength: Wavelength,
    lineWidth:  LineWidthValue,
    lineFlux:   LineFluxValue,
    units:      Units
  )

  override def toString: String =
    this match
      case ForBand(band, value, units)                             =>
        f"${band.shortName}: $value%.2f  $units"
      case ForEmissionLine(wavelength, lineWidth, lineFlux, units) =>
        f"${wavelength.toNanometers}%.0f nm: $lineWidth%.2f km/s, ${lineFlux.shortName} $units"
