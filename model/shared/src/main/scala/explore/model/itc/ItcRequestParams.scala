// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.itc

import cats.data.*
import explore.modes.InstrumentConfig
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet

case class ItcRequestParams(
  atWavelength:  Wavelength,
  signalToNoise: SignalToNoise,
  constraints:   ConstraintSet,
  asterism:      NonEmptyList[ItcTarget],
  mode:          InstrumentConfig
)

case class ItcGraphRequestParams(
  atWavelength:  Wavelength,
  signalToNoise: SignalToNoise,
  constraints:   ConstraintSet,
  asterism:      NonEmptyList[ItcTarget],
  mode:          InstrumentConfig
)
