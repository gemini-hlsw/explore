// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Order._
import cats._
import lucuma.core.model.Target
import monocle.Focus

import scala.collection.immutable.SortedSet

case class ExpandedIds(
  targetIds:           SortedSet[Target.Id] = SortedSet.empty,
  constraintSetObsIds: SortedSet[ObsIdSet] = SortedSet.empty,
  asterismObsIds:      SortedSet[ObsIdSet] = SortedSet.empty
)

object ExpandedIds {
  val targetIds           = Focus[ExpandedIds](_.targetIds)
  val constraintSetObsIds = Focus[ExpandedIds](_.constraintSetObsIds)
  val asterismObsIds      = Focus[ExpandedIds](_.asterismObsIds)

  implicit val eqExpandedIds: Eq[ExpandedIds] = Eq.fromUniversalEquals
}
