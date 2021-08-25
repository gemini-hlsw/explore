// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Order._
import cats._
import lucuma.core.model.Asterism
import lucuma.core.model.Observation
import lucuma.core.model.Target
import monocle.Focus

import scala.collection.immutable.SortedSet

case class ExpandedIds(
  targetIds:           SortedSet[Target.Id] = SortedSet.empty,
  asterismIds:         SortedSet[Asterism.Id] = SortedSet.empty,
  constraintSetObsIds: SortedSet[SortedSet[Observation.Id]] = SortedSet.empty
)

object ExpandedIds {
  val targetIds           = Focus[ExpandedIds](_.targetIds)
  val asterismIds         = Focus[ExpandedIds](_.asterismIds)
  val constraintSetObsIds = Focus[ExpandedIds](_.constraintSetObsIds)

  implicit val eqExpandedIds: Eq[ExpandedIds] = Eq.fromUniversalEquals
}
