// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import io.circe.Decoder
import lucuma.core.util.TimeSpan

case class ProgramTimes(
  timeEstimateRange:  Option[ProgramTimeRange],
  timeEstimateBanded: List[BandedProgramTime],
  timeCharge:         List[BandedProgramTime]
) derives Eq,
      Decoder:
  val fullProgramTime: TimeSpan =
    timeCharge.foldLeft(TimeSpan.Zero)((acc, bpt) => acc +| bpt.time.value)
