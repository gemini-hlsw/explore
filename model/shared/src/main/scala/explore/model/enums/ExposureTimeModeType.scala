// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.refined.*

enum ExposureTimeModeType(val label: NonEmptyString) derives Enumerated:
  case SignalToNoise extends ExposureTimeModeType("Signal / Noise".refined)
  case TimeAndCount  extends ExposureTimeModeType("Time & Count".refined)

  private val tag = label.value

object ExposureTimeModeType:
  def fromExposureTimeMode(etm: ExposureTimeMode): ExposureTimeModeType = etm match {
    case ExposureTimeMode.SignalToNoiseMode(_, _)   => SignalToNoise
    case ExposureTimeMode.TimeAndCountMode(_, _, _) => TimeAndCount
  }

  given Display[ExposureTimeModeType] = Display.byShortName(_.label.value)

  extension (mode: ExposureTimeMode)
    def modeType: ExposureTimeModeType = mode match
      case ExposureTimeMode.SignalToNoiseMode(_, _)   => ExposureTimeModeType.SignalToNoise
      case ExposureTimeMode.TimeAndCountMode(_, _, _) => ExposureTimeModeType.TimeAndCount
