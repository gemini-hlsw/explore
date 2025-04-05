// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.itc.renderRequiredForITCIcon
import explore.model.Constants
import explore.model.ExploreModelValidators
import explore.model.ScienceRequirements
import explore.model.ScienceRequirements.SignalToNoiseModeInfo
import explore.model.enums.WavelengthUnits
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.validation.*
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.FormLabel
import lucuma.ui.primereact.clearable
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

case class SignalToNoiseAtEditor(
  options:         View[SignalToNoiseModeInfo],
  readonly:        Boolean,
  units:           WavelengthUnits,
  calibrationRole: Option[CalibrationRole]
) extends ReactFnProps[SignalToNoiseAtEditor](SignalToNoiseAtEditor.component)

object SignalToNoiseAtEditor extends ConfigurationFormats {
  private type Props = SignalToNoiseAtEditor

  protected val component =
    ScalaFnComponent[Props] { props =>
      val signalToNoise   = props.options.zoom(SignalToNoiseModeInfo.value)
      val signalToNoiseAt = props.options.zoom(SignalToNoiseModeInfo.at)

      React.Fragment(
        FormLabel("signal-to-noise".refined)(
          "Signal / Noise",
          HelpIcon("configuration/signal_to_noise.md".refined)
        ),
        FormInputTextView(
          id = "signal-to-noise".refined,
          value = signalToNoise,
          groupClass = ExploreStyles.WarningInput.when_(signalToNoise.get.isEmpty),
          validFormat = ExploreModelValidators.signalToNoiseValidSplitEpi.optional,
          postAddons =
            signalToNoise.get.fold(List(props.calibrationRole.renderRequiredForITCIcon))(_ => Nil),
          changeAuditor = ChangeAuditor.posBigDecimal(1.refined).optional,
          disabled = props.readonly
        ).withMods(^.autoComplete.off),
        FormLabel("signal-to-noise-at".refined)(Constants.SignalToNoiseAtLabel),
        FormInputTextView(
          id = "signal-to-noise-at".refined,
          groupClass = ExploreStyles.WarningInput.when_(signalToNoiseAt.get.isEmpty),
          postAddons =
            signalToNoiseAt.get.fold(List(props.calibrationRole.renderRequiredForITCIcon))(_ =>
              Nil
            ),
          value = signalToNoiseAt,
          units = props.units.symbol,
          validFormat = props.units.toInputWedge,
          changeAuditor = props.units.toSNAuditor,
          disabled = props.readonly
        ).clearable(^.autoComplete.off)
      )
    }
}
