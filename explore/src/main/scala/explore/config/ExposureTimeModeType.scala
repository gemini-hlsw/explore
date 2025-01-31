// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.refined.*

enum ExposureTimeModeType(val label: NonEmptyString) derives Enumerated:
  case SignalToNoise extends ExposureTimeModeType("S/N".refined)
  case FixedExposure extends ExposureTimeModeType("Fixed".refined)

  private val tag = label.value

object ExposureTimeModeType:
  def fromExposureTimeMode(etm: ExposureTimeMode): ExposureTimeModeType = etm match {
    case ExposureTimeMode.SignalToNoiseMode(_, _)   => SignalToNoise
    case ExposureTimeMode.TimeAndCountMode(_, _, _) => FixedExposure
  }

  given Display[ExposureTimeModeType] = Display.byShortName(_.label.value)

  given Reusability[ExposureTimeModeType] = Reusability.byEq
