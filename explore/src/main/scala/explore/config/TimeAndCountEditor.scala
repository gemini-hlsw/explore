// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.itc.renderRequiredForITCIcon
import explore.model.ScienceRequirements
import explore.model.ScienceRequirements.TimeAndCountModeInfo
import explore.model.enums.WavelengthUnits
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
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
import explore.model.formats.durationMs

case class TimeAndCountEditor(
  options:         View[TimeAndCountModeInfo],
  readonly:        Boolean,
  units:           WavelengthUnits,
  calibrationRole: Option[CalibrationRole]
) extends ReactFnProps[TimeAndCountEditor](TimeAndCountEditor.component)

object TimeAndCountEditor extends ConfigurationFormats {
  private type Props = TimeAndCountEditor

  protected val component =
    ScalaFnComponent[Props] { props =>
      val exposureTime    = props.options.zoom(TimeAndCountModeInfo.time)
      val signalToNoiseAt = props.options.zoom(TimeAndCountModeInfo.at)

      println(exposureTime.get)
      React.Fragment(
        FormLabel("signal-to-noise".refined)(
          "Time & Count",
          HelpIcon("configuration/time_and_count.md".refined)
        ),
        <.div(
          LucumaPrimeStyles.FormField |+| ExploreStyles.BasicConfigurationSNAt,
          FormInputTextView(
            id = "exposure-time".refined,
            value = exposureTime,
            groupClass = ExploreStyles.WarningInput.when_(exposureTime.get.isEmpty),
            validFormat = durationMs.optional,
            postAddons =
              exposureTime.get.fold(List(props.calibrationRole.renderRequiredForITCIcon))(_ => Nil),
            changeAuditor = ChangeAuditor.posBigDecimal(3.refined).optional,
            units = "s",
            disabled = props.readonly
          ).withMods(^.autoComplete.off),
          FormLabel("signal-to-noise-at".refined)("at"),
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
      )
    }
}
