// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import io.circe.Decoder
import lucuma.core.util.CalculatedValue
import lucuma.core.util.TimeSpan
import lucuma.schemas.decoders.given

case class ProgramTimes(
  timeEstimateRange:  CalculatedValue[Option[ProgramTimeRange]],
  timeEstimateBanded: List[CalculatedValue[BandedProgramTime]],
  timeCharge:         List[BandedProgramTime]
) derives Eq,
      Decoder:
  val fullProgramTime: TimeSpan =
    timeCharge.foldLeft(TimeSpan.Zero)((acc, bpt) => acc +| bpt.time.value)
