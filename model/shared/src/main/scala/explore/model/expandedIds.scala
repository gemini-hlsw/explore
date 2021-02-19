// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Order._
import cats._
import lucuma.core.model.Asterism
import lucuma.core.model.Target
import monocle.macros.Lenses

import scala.collection.immutable.SortedSet

@Lenses
case class TargetViewExpandedIds(
  targetIds:   SortedSet[Target.Id] = SortedSet.empty,
  asterismIds: SortedSet[Asterism.Id] = SortedSet.empty)

object TargetViewExpandedIds {
  implicit val eqTargetViewExpandedIds: Eq[TargetViewExpandedIds] =
    Eq.by(m => (m.targetIds, m.asterismIds))
}
