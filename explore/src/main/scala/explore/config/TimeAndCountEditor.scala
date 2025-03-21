// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.ui.ExploreStyles
import explore.itc.renderRequiredForITCIcon
import explore.model.ExploreModelValidators
import explore.model.ScienceRequirements
import explore.model.ScienceRequirements.TimeAndCountModeInfo
import explore.model.enums.WavelengthUnits
import explore.model.formats.durationMs
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
import lucuma.ui.primereact.clearable
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

case class TimeAndCountEditor(
  options:         View[TimeAndCountModeInfo],
  readonly:        Boolean,
  units:           WavelengthUnits,
  calibrationRole: Option[CalibrationRole]
) extends ReactFnProps[TimeAndCountEditor](TimeAndCountEditor.component)

object TimeAndCountEditor extends ConfigurationFormats:
  private type Props = TimeAndCountEditor

  protected val component =
    ScalaFnComponent[Props]: props =>
      val exposureTime    = props.options.zoom(TimeAndCountModeInfo.time)
      val count           = props.options.zoom(TimeAndCountModeInfo.count)
      val signalToNoiseAt = props.options.zoom(TimeAndCountModeInfo.at)

      React.Fragment(
        FormLabel("signal-to-noise-at".refined)("S/N at"),
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
        ).clearable(^.autoComplete.off),
        FormLabel("exposure-time".refined)("Exp. Time"),
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
        FormLabel("signal-to-noise".refined)("Exp. Count"),
        FormInputTextView(
          id = "count".refined,
          value = count,
          groupClass = ExploreStyles.WarningInput.when_(count.get.isEmpty),
          validFormat = ExploreModelValidators.nonNegInt.optional,
          postAddons =
            count.get.fold(List(props.calibrationRole.renderRequiredForITCIcon))(_ => Nil),
          changeAuditor = ChangeAuditor.int.optional,
          units = "#",
          disabled = props.readonly
        ).withMods(^.autoComplete.off)
      )
