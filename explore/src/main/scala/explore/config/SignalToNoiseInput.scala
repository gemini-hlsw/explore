// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import crystal.react.View
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.itc.renderRequiredForITCIcon
import explore.model.ExploreModelValidators
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.math.SignalToNoise
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

case class SignalToNoiseInput(
  signalToNoise:   View[Option[SignalToNoise]],
  calibrationRole: Option[CalibrationRole],
  readonly:        Boolean
) extends ReactFnProps(SignalToNoiseInput)

object SignalToNoiseInput
    extends ReactFnComponent[SignalToNoiseInput](props =>
      val groupClass = ExploreStyles.WarningInput.when_(props.signalToNoise.get.isEmpty)
      val postAddons =
        props.signalToNoise.get.fold(List(props.calibrationRole.renderRequiredForITCIcon))(_ => Nil)

      React.Fragment(
        FormInputTextView(
          id = "signal-to-noise".refined,
          value = props.signalToNoise,
          label =
            React.Fragment("Signal / Noise", HelpIcon("configuration/signal_to_noise.md".refined)),
          groupClass = groupClass,
          validFormat = ExploreModelValidators.signalToNoiseValidSplitEpi.optional,
          postAddons = postAddons,
          changeAuditor = ChangeAuditor.posBigDecimal(1.refined).optional,
          disabled = props.readonly
        ).withMods(^.autoComplete.off)
      )
    )
