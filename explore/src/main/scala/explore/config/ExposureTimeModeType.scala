// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.ReactCats._
import japgolly.scalajs.react.Reusability
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.refined.*

enum ExposureTimeModeType(val label: NonEmptyString):
  case SignalToNoise extends ExposureTimeModeType("S/N".refined)
  case FixedExposure extends ExposureTimeModeType("Fixed".refined)

object ExposureTimeModeType:
  def fromExposureTimeMode(etm: ExposureTimeMode): ExposureTimeModeType = etm match {
    case ExposureTimeMode.SignalToNoise(_)    => SignalToNoise
    case ExposureTimeMode.FixedExposure(_, _) => FixedExposure
  }

  given Enumerated[ExposureTimeModeType] =
    Enumerated.from(SignalToNoise, FixedExposure).withTag(_.label.value)

  given Display[ExposureTimeModeType] = Display.byShortName(_.label.value)

  given Reusability[ExposureTimeModeType] = Reusability.byEq
