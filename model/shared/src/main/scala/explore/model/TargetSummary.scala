// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.*
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.schemas.model.TargetWithId

case class TargetSummary(obsIds: Set[Observation.Id], target: TargetWithId) derives Eq
