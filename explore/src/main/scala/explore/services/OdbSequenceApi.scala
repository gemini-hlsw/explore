// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import lucuma.core.model.Observation
import lucuma.ui.sequence.SequenceData

trait OdbSequenceApi[F[_]]:
  def sequenceData(obsId: Observation.Id, includeItc: Boolean): F[Option[SequenceData]]
