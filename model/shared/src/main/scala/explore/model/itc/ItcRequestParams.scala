// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.itc

import cats.data.*
import explore.modes.InstrumentConfig
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.Timestamp

case class ItcRequestParams(
  exposureTimeMode:    ExposureTimeMode,
  constraints:         ConstraintSet,
  asterism:            NonEmptyList[ItcTarget],
  customSedTimestamps: List[Timestamp],
  mode:                InstrumentConfig
)

case class ItcGraphRequestParams(
  exposureTimeMode:    ExposureTimeMode,
  constraints:         ConstraintSet,
  asterism:            NonEmptyList[ItcTarget],
  customSedTimestamps: List[Timestamp],
  mode:                InstrumentConfig
)
