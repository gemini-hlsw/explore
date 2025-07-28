// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.cats.*
import explore.components.ui.ExploreStyles
import explore.itc.renderRequiredForITCIcon
import explore.model.Constants
import explore.model.TimeAndCountModeInfo
import explore.model.enums.WavelengthUnits
import explore.model.formats.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Instrument
import lucuma.core.enums.ScienceMode
import lucuma.core.math.Wavelength
import lucuma.core.validation.*
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.FormLabel
import lucuma.ui.primereact.clearable
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

case class TimeAndCountEditor(
  instrument:      Option[Instrument],
  options:         View[TimeAndCountModeInfo],
  scienceMode:     ScienceMode,
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

      val (timeFormat, timeAuditor) = props.instrument
        .collect { case Instrument.GmosSouth | Instrument.GmosNorth | Instrument.Flamingos2 =>
          (durationS.optional, ChangeAuditor.int.optional)
        }
        .getOrElse((durationMs.optional, ChangeAuditor.posBigDecimal(2.refined).optional))

      React.Fragment(
        FormLabel("exposure-time".refined)("Exp. Time"),
        FormInputTextView(
          id = "exposure-time".refined,
          value = exposureTime,
          groupClass = ExploreStyles.WarningInput.when_(exposureTime.get.isEmpty),
          validFormat = timeFormat,
          postAddons =
            exposureTime.get.fold(List(props.calibrationRole.renderRequiredForITCIcon))(_ => Nil),
          units = "s",
          changeAuditor = timeAuditor,
          disabled = props.readonly
        ).clearable(^.autoComplete.off),
        FormLabel("exposure-count".refined)("Number of Exp."),
        FormInputTextView(
          id = "exposure-count".refined,
          value = count,
          groupClass = ExploreStyles.WarningInput.when_(count.get.isEmpty),
          validFormat = InputValidSplitEpi.posInt.optional,
          postAddons =
            count.get.fold(List(props.calibrationRole.renderRequiredForITCIcon))(_ => Nil),
          changeAuditor = ChangeAuditor.int.optional,
          units = "#",
          disabled = props.readonly
        ).clearable(^.autoComplete.off),
        Option.when(props.scienceMode === ScienceMode.Spectroscopy):
          React
            .Fragment(
              FormLabel("signal-to-noise-at".refined)(Constants.SignalToNoiseAtLabel),
              FormInputTextView(
                id = "signal-to-noise-at".refined,
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
