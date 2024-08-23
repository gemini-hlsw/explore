// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.itc

import cats.data.*
import eu.timepit.refined.types.numeric.PosInt
import explore.modes.InstrumentRow
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.util.TimeSpan
import lucuma.schemas.model.CentralWavelength

case class ItcRequestParams(
  wavelength:      CentralWavelength,
  signalToNoise:   SignalToNoise,
  signalToNoiseAt: Wavelength,
  constraints:     ConstraintSet,
  target:          ItcTarget,
  mode:            InstrumentRow
)

case class ItcGraphRequestParams(
  wavelength:      CentralWavelength,
  exposureTime:    TimeSpan,
  exposures:       PosInt,
  signalToNoiseAt: Wavelength,
  constraints:     ConstraintSet,
  target:          NonEmptyList[ItcTarget],
  mode:            InstrumentRow
)
