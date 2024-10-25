// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import io.circe.Decoder
import lucuma.core.enums.ScienceBand

final case class BandedProgramTime(
  band: ScienceBand,
  time: ProgramTime
) derives Eq,
      Decoder