// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import crystal.react.hooks.*
import explore.components.HelpIcon
import explore.model.SignalToNoiseModeInfo
import explore.model.TimeAndCountModeInfo
import explore.model.enums.ExposureTimeModeType
import explore.model.enums.ExposureTimeModeType.*
import explore.model.enums.WavelengthUnits
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Instrument
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given

case class ExposureTimeModeEditor(
  instrument:       Option[Instrument],
  wavelength:       Option[Wavelength],
  exposureTimeMode: View[Option[ExposureTimeMode]],
  readonly:         Boolean,
  units:            WavelengthUnits,
  calibrationRole:  Option[CalibrationRole]
) extends ReactFnProps[ExposureTimeModeEditor](ExposureTimeModeEditor.component)

object ExposureTimeModeEditor:
  private type Props = ExposureTimeModeEditor

  protected val component =
    ScalaFnComponent[Props]: props =>
      for
        emv    <-
          useStateView(
            props.exposureTimeMode.get
              .map(_.modeType)
              .getOrElse(ExposureTimeModeType.SignalToNoise)
          )
        snMode <- useStateView(
                    props.exposureTimeMode.get
                      .flatMap(SignalToNoiseModeInfo.fromModel)
                      .getOrElse(SignalToNoiseModeInfo.Default)
                  )
        tcMode <- useStateView(
                    props.exposureTimeMode.get
                      .flatMap(TimeAndCountModeInfo.fromModel)
                      .getOrElse(TimeAndCountModeInfo.Default)
                  )
        _      <- useEffectWithDeps(props.exposureTimeMode.get):
                    // Exposure time mode updated upstream
                    _.map: etm =>
                      SignalToNoiseModeInfo.fromModel(etm).traverse(snMode.set) *>
                        TimeAndCountModeInfo.fromModel(etm).traverse(tcMode.set) *>
                        emv.set(etm.modeType)
                    .getOrElse:
                      snMode.set(SignalToNoiseModeInfo.Default) *>
                        tcMode.set(TimeAndCountModeInfo.Default) *>
                        emv.set(ExposureTimeModeType.SignalToNoise)
        _      <- useEffectWithDeps(props.wavelength):
                    // Wavelength updated upstream, set `at` if empty
                    _.map: wv =>
                      emv.get match
                        case ExposureTimeModeType.SignalToNoise =>
                          snMode.set(snMode.get.withRequirementsWavelength(wv))
                        case ExposureTimeModeType.TimeAndCount  =>
                          tcMode.set(tcMode.get.withRequirementsWavelength(wv))
                    .getOrEmpty
      yield

        val snModeView = snMode.withOnMod:
          case SignalToNoiseModeInfo(Some(value), Some(at)) =>
            props.exposureTimeMode.set(ExposureTimeMode.SignalToNoiseMode(value, at).some)
          case _                                            =>
            Callback.empty

        val tcModeView = tcMode.withOnMod:
          case TimeAndCountModeInfo(Some(time), Some(count), Some(at)) =>
            props.exposureTimeMode.set(ExposureTimeMode.TimeAndCountMode(time, count, at).some)
          case _                                                       =>
            Callback.empty

        React.Fragment(
          FormEnumDropdownView(
            id = "exposureMode".refined,
            value = emv,
            label = ReactFragment(
              "Exposure Mode",
              HelpIcon("configuration/exposure-mode.md".refined)
            ),
            onChangeE = (v, _) =>
              v match
                case Some(ExposureTimeModeType.SignalToNoise) =>
                  tcMode.get match
                    case TimeAndCountModeInfo(_, _, Some(at)) =>
                      snModeView.mod:
                        case s @ SignalToNoiseModeInfo(_, None) => s.copy(at = Some(at))
                        case s                                  => s
                    case _                                    => Callback.empty
                case Some(ExposureTimeModeType.TimeAndCount)  =>
                  snMode.get match
                    case SignalToNoiseModeInfo(_, Some(at)) =>
                      tcModeView.mod:
                        case s @ TimeAndCountModeInfo(_, _, None) => s.copy(at = Some(at))
                        case s                                    => s
                    case _                                  => Callback.empty
                case None                                     => Callback.empty
            ,
            disabled = props.readonly
          ),
          if (emv.get === ExposureTimeModeType.SignalToNoise)
            SignalToNoiseAtEditor(snModeView, props.readonly, props.units, props.calibrationRole)
          else
            TimeAndCountEditor(props.instrument,
                               tcModeView,
                               props.readonly,
                               props.units,
                               props.calibrationRole
            )
        )
