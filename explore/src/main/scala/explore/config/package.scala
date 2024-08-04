// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.itc.requiredForITC
import explore.model.ExploreModelValidators
import explore.model.ScienceRequirements
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
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
  lazy val wvMicroInput                  = ExploreModelValidators.wavelengthValidWedge.optional
  lazy val wvcMicroInput                 = ExploreModelValidators.wavelengthDeltaValidWedge.optional
  lazy val wvBaseAuditor                 = ChangeAuditor
    .fromInputValidWedge(ExploreModelValidators.wavelengthValidWedge)
    .allow(s => s === "0" || s === "0.")
  lazy val wvChangeAuditor               = wvBaseAuditor
    .decimal(3.refined)
    .optional
  lazy val snAtWvChangeAuditor           = wvBaseAuditor
    .decimal(4.refined)
    .optional

case class SignalToNoiseAt(
  options:  View[ScienceRequirements.Spectroscopy],
  readonly: Boolean
) extends ReactFnProps[SignalToNoiseAt](SignalToNoiseAt.component)

object SignalToNoiseAt extends ConfigurationFormats {
  private type Props = SignalToNoiseAt

  protected val component =
    ScalaFnComponent[Props] { props =>
      val signalToNoise   = props.options.zoom(ScienceRequirements.Spectroscopy.signalToNoise)
      val signalToNoiseAt = props.options.zoom(ScienceRequirements.Spectroscopy.signalToNoiseAt)
      React.Fragment(
        FormLabel("signal-to-noise".refined)(
          "S / N",
          HelpIcon("configuration/signal_to_noise.md".refined)
        ),
        <.div(
          LucumaPrimeStyles.FormField |+| ExploreStyles.BasicConfigurationSNAt,
          FormInputTextView(
            id = "signal-to-noise".refined,
            value = signalToNoise,
            groupClass = ExploreStyles.WarningInput.when_(signalToNoise.get.isEmpty),
            validFormat = ExploreModelValidators.signalToNoiseValidSplitEpi.optional,
            postAddons = signalToNoise.get.fold(List(requiredForITC))(_ => Nil),
            changeAuditor = ChangeAuditor.posBigDecimal(1.refined).optional,
            disabled = props.readonly
          ).withMods(^.autoComplete.off),
          FormLabel("signal-to-noise-at".refined)("at"),
          FormInputTextView(
            id = "signal-to-noise-at".refined,
            groupClass = ExploreStyles.WarningInput.when_(signalToNoiseAt.get.isEmpty),
            postAddons = signalToNoiseAt.get.fold(List(requiredForITC))(_ => Nil),
            value = signalToNoiseAt,
            units = "μm",
            validFormat = wvMicroInput,
            changeAuditor = snAtWvChangeAuditor,
            disabled = props.readonly
          ).clearable(^.autoComplete.off)
        )
      )
    }
}
