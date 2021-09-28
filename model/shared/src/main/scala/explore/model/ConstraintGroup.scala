// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import lucuma.core.model.Observation
import monocle.Focus

import scala.collection.immutable.SortedSet

final case class ConstraintGroup(constraintSet: ConstraintSet, obsIds: SortedSet[Observation.Id]) {
  def addObsId(obsId: Observation.Id): ConstraintGroup =
    ConstraintGroup.obsIds.modify(_ + obsId)(this)
  def removeObsId(obsId: Observation.Id): ConstraintGroup =
    ConstraintGroup.obsIds.modify(_ - obsId)(this)
  def asKeyValue: (SortedSet[Observation.Id], ConstraintGroup) = (this.obsIds, this)
}

object ConstraintGroup {
  val constraintSet = Focus[ConstraintGroup](_.constraintSet)
  val obsIds        = Focus[ConstraintGroup](_.obsIds)

  implicit val constraintGroupEq: Eq[ConstraintGroup] = Eq.by(cg => (cg.constraintSet, cg.obsIds))
}
