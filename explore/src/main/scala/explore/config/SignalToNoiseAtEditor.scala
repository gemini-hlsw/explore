// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import crystal.react.View
import explore.components.ui.ExploreStyles
import explore.config.ConfigurationFormats.*
import explore.itc.renderRequiredForITCIcon
import explore.model.Constants
import explore.model.SignalToNoiseModeInfo
import explore.model.enums.WavelengthUnits
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.math.Wavelength
import lucuma.core.validation.*
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.clearable
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

case class SignalToNoiseAtEditor(
  options:         View[SignalToNoiseModeInfo],
  readonly:        Boolean,
  units:           WavelengthUnits,
  calibrationRole: Option[CalibrationRole]
) extends ReactFnProps(SignalToNoiseAtEditor)

object SignalToNoiseAtEditor
    extends ReactFnComponent[SignalToNoiseAtEditor](props =>
      val signalToNoise   = props.options.zoom(SignalToNoiseModeInfo.value)
      val signalToNoiseAt = props.options.zoom(SignalToNoiseModeInfo.at)

      React.Fragment(
        SignalToNoiseInput(
          signalToNoise,
          props.calibrationRole,
          props.readonly
        ),
        FormInputTextView(
          id = "signal-to-noise-at".refined,
          label = Constants.SignalToNoiseAtLabel,
          groupClass = ExploreStyles.WarningInput.when_(signalToNoiseAt.get.isEmpty),
          postAddons = signalToNoiseAt.get.fold(
            List(props.calibrationRole.renderRequiredForITCIcon)
          )(_ => Nil),
          value = signalToNoiseAt,
          units = props.units.symbol,
          validFormat = props.units.toInputWedge,
          changeAuditor = props.units.toSNAuditor,
          disabled = props.readonly
        ).clearable(^.autoComplete.off)
      )
    )
