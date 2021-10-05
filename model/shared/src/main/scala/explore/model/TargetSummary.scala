// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
// import cats.syntax.all._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
// import lucuma.core.model.Asterism
import lucuma.core.model.Target

// trait TargetSummary {
//   val id: Target.Id
//   val name: NonEmptyString
// }

case class TargetSummary(id: Target.Id, val name: NonEmptyString)

object TargetSummary {
  implicit val targetSummaryEq: Eq[TargetSummary] = Eq.by(t => (t.id, t.name))
}
