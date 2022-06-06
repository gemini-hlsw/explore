// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import eu.timepit.refined.cats._
import lucuma.core.math.Coordinates
import lucuma.core.model.Observation
import lucuma.core.model.Target

case class TargetSummary(
  obsIds:   Set[Observation.Id],
  targetId: Target.Id,
  coords:   Option[Coordinates]
)

object TargetSummary {
  implicit val targetSummaryEq: Eq[TargetSummary] = Eq.by(t => (t.obsIds, t.targetId, t.coords))
}
