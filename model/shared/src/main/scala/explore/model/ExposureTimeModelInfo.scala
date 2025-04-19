// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.ExposureTimeMode.SignalToNoiseMode
import lucuma.core.model.ExposureTimeMode.TimeAndCountMode
import lucuma.core.util.TimeSpan
import monocle.Focus
import monocle.Lens

case class TimeAndCountModeInfo(
  time:  Option[TimeSpan],
  count: Option[NonNegInt],
  at:    Option[Wavelength]
) derives Eq:
  def withRequirementsWavelength(w: Wavelength): TimeAndCountModeInfo =
    at.fold(copy(at = w.some))(_ => this)

object TimeAndCountModeInfo:
  val time: Lens[TimeAndCountModeInfo, Option[TimeSpan]] =
    Focus[TimeAndCountModeInfo](_.time)

  val count: Lens[TimeAndCountModeInfo, Option[NonNegInt]] =
    Focus[TimeAndCountModeInfo](_.count)

  val at: Lens[TimeAndCountModeInfo, Option[Wavelength]] =
    Focus[TimeAndCountModeInfo](_.at)

  val Default = TimeAndCountModeInfo(None, None, None)

  def fromModel(etm: ExposureTimeMode): Option[TimeAndCountModeInfo] =
    etm match
      case ExposureTimeMode.TimeAndCountMode(t, c, a) =>
        TimeAndCountModeInfo(t.some, c.some, a.some).some
      case _                                          => none

case class SignalToNoiseModeInfo(value: Option[SignalToNoise], at: Option[Wavelength]) derives Eq:
  def withRequirementsWavelength(w: Wavelength): SignalToNoiseModeInfo =
    at.fold(copy(at = w.some))(_ => this)

object SignalToNoiseModeInfo:
  val value: Lens[SignalToNoiseModeInfo, Option[SignalToNoise]] =
    Focus[SignalToNoiseModeInfo](_.value)

  val at: Lens[SignalToNoiseModeInfo, Option[Wavelength]] =
    Focus[SignalToNoiseModeInfo](_.at)

  val Default = SignalToNoiseModeInfo(None, None)

  def fromModel(etm: ExposureTimeMode): Option[SignalToNoiseModeInfo] =
    etm match
      case ExposureTimeMode.SignalToNoiseMode(v, a) =>
        SignalToNoiseModeInfo(v.some, a.some).some
      case _                                        => none
