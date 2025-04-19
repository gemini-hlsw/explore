// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.*
import cats.Order.*
import cats.derived.*
import lucuma.core.model.Group
import lucuma.core.model.Target
import monocle.Focus

import scala.collection.immutable.SortedSet

case class ExpandedIds(
  targetIds:           SortedSet[Target.Id] = SortedSet.empty,
  constraintSetObsIds: SortedSet[ObsIdSet] = SortedSet.empty,
  schedulingObsIds:    SortedSet[ObsIdSet] = SortedSet.empty,
  asterismObsIds:      SortedSet[ObsIdSet] = SortedSet.empty,
  obsListGroupIds:     Set[Group.Id] = Set.empty
) derives Eq

object ExpandedIds {
  val targetIds           = Focus[ExpandedIds](_.targetIds)
  val constraintSetObsIds = Focus[ExpandedIds](_.constraintSetObsIds)
  val schedulingObsIds    = Focus[ExpandedIds](_.schedulingObsIds)
  val asterismObsIds      = Focus[ExpandedIds](_.asterismObsIds)
  val obsListGroupIds     = Focus[ExpandedIds](_.obsListGroupIds)

}
