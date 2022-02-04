// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.Target
import lucuma.core.math.Coordinates

case class TargetSummary(id: Target.Id, name: NonEmptyString, coords: Option[Coordinates])

object TargetSummary {
  implicit val targetSummaryEq: Eq[TargetSummary] = Eq.by(t => (t.id, t.name, t.coords))
}
