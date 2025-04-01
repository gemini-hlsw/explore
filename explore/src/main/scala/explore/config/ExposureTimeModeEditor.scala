// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import explore.components.HelpIcon
import explore.model.ScienceRequirements
import explore.model.ScienceRequirements.ExposureTimeModeInfo
import explore.model.enums.ExposureTimeModeType
import explore.model.enums.WavelengthUnits
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Instrument
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import monocle.Lens

case class ExposureTimeModeEditor(
  instrument:      Option[Instrument],
  options:         View[ExposureTimeModeInfo],
  readonly:        Boolean,
  units:           WavelengthUnits,
  calibrationRole: Option[CalibrationRole]
) extends ReactFnProps[ExposureTimeModeEditor](ExposureTimeModeEditor.component)

object ExposureTimeModeEditor:
  private type Props = ExposureTimeModeEditor

  // This is not a lawful lens, it will try to convert one mode to the next preserving the data that makes sense
  val emvLens: Lens[ExposureTimeModeInfo, ExposureTimeModeType] =
    Lens[ExposureTimeModeInfo, ExposureTimeModeType](_.exposureMode)(a =>
      p =>
        a match
          case ExposureTimeModeType.SignalToNoise => p.asSignalToNoiseMode
          case ExposureTimeModeType.TimeAndCount  => p.asTimeAndCountMode
    )

  protected val component =
    ScalaFnComponent[Props]: props =>

      val snMode = props.options.zoom(ExposureTimeModeInfo.signalToNoise)
      val tcMode = props.options.zoom(ExposureTimeModeInfo.timeAndCount)

      val emv = props.options.zoom(emvLens)

      React.Fragment(
        FormEnumDropdownView(
          id = "exposureMode".refined,
          value = emv,
          label = ReactFragment(
            "Exposure Mode",
            HelpIcon("configuration/exposure-mode.md".refined)
          ),
          disabled = props.readonly
        ),
        snMode.asView.map(
          SignalToNoiseAtEditor(_, props.readonly, props.units, props.calibrationRole)
        ),
        tcMode.asView.map(
          TimeAndCountEditor(props.instrument,
                             _,
                             props.readonly,
                             props.units,
                             props.calibrationRole
          )
        )
      )
