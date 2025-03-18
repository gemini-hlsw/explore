// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.itc.renderRequiredForITCIcon
import explore.model.ExploreModelValidators
import explore.model.ScienceRequirements
import explore.model.enums.WavelengthUnits
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.validation.*
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.FormLabel
import lucuma.ui.primereact.LucumaPrimeStyles
import lucuma.ui.primereact.clearable
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

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
  lazy val wvMicroBaseAuditor            = ChangeAuditor
    .fromInputValidWedge(ExploreModelValidators.wavelengthMicroValidWedge)
    .allow(s => s === "0" || s === "0.")
  lazy val wvMicroChangeAuditor          = wvMicroBaseAuditor
    .decimal(4.refined)
    .optional
  lazy val snAtWvMicroChangeAuditor      = wvMicroBaseAuditor
    .decimal(4.refined)
    .optional
  lazy val wvNanoBaseAuditor             = ChangeAuditor
    .fromInputValidWedge(ExploreModelValidators.wavelengthNanoValidWedge)
    .allow(s => s === "0" || s === "0.")
  lazy val wvNanoChangeAuditor           = wvNanoBaseAuditor.decimal(1.refined).optional
  lazy val snAtWvNanoChangeAuditor       = wvNanoBaseAuditor.decimal(1.refined).optional

  extension (u: WavelengthUnits)
    def toAuditor: ChangeAuditor =
      u match
        case WavelengthUnits.Micrometers => wvMicroChangeAuditor
        case WavelengthUnits.Nanometers  => wvNanoChangeAuditor

    def toSNAuditor: ChangeAuditor =
      u match
        case WavelengthUnits.Micrometers => snAtWvMicroChangeAuditor
        case WavelengthUnits.Nanometers  => snAtWvNanoChangeAuditor

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

case class SignalToNoiseAt(
  options:         View[ScienceRequirements.Spectroscopy],
  readonly:        Boolean,
  units:           WavelengthUnits,
  calibrationRole: Option[CalibrationRole]
) extends ReactFnProps[SignalToNoiseAt](SignalToNoiseAt.component)

object SignalToNoiseAt extends ConfigurationFormats {
  private type Props = SignalToNoiseAt

  protected val component =
    ScalaFnComponent[Props] { props =>
      // val signalToNoise   = props.options.zoom(ScienceRequirements.Spectroscopy.signalToNoise)
      // val signalToNoiseAt = props.options.zoom(ScienceRequirements.Spectroscopy.signalToNoiseAt)
      React.Fragment(
        FormLabel("signal-to-noise".refined)(
          "S / N",
          HelpIcon("configuration/signal_to_noise.md".refined)
        ),
        <.div(
          LucumaPrimeStyles.FormField |+| ExploreStyles.BasicConfigurationSNAt,
          // FormInputTextView(
          //   id = "signal-to-noise".refined,
          //   value = signalToNoise,
          //   groupClass = ExploreStyles.WarningInput.when_(signalToNoise.get.isEmpty),
          //   validFormat = ExploreModelValidators.signalToNoiseValidSplitEpi.optional,
          //   postAddons =
          //     signalToNoise.get.fold(List(props.calibrationRole.renderRequiredForITCIcon))(_ =>
          //       Nil
          //     ),
          //   changeAuditor = ChangeAuditor.posBigDecimal(3.refined).optional,
          //   disabled = props.readonly
          // ).withMods(^.autoComplete.off),
          FormLabel("signal-to-noise-at".refined)("at")
          //   FormInputTextView(
          //     id = "signal-to-noise-at".refined,
          //     groupClass = ExploreStyles.WarningInput.when_(signalToNoiseAt.get.isEmpty),
          //     postAddons =
          //       signalToNoiseAt.get.fold(List(props.calibrationRole.renderRequiredForITCIcon))(_ =>
          //         Nil
          //       ),
          //     value = signalToNoiseAt,
          //     units = props.units.symbol,
          //     validFormat = props.units.toInputWedge,
          //     changeAuditor = props.units.toSNAuditor,
          //     disabled = props.readonly
          //   ).clearable(^.autoComplete.off)
        )
      )
    }
}
