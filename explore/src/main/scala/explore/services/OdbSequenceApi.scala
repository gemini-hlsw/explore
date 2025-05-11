// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import explore.model.SequenceData
import lucuma.core.model.Observation

trait OdbSequenceApi[F[_]]:
  def sequenceData(obsId: Observation.Id): F[Option[SequenceData]]
