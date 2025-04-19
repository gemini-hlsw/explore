// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import explore.model.syntax.all.*

case class ObsIdSetEditInfo(
  editing:     ObsIdSet,
  executed:    Option[ObsIdSet],
  asterismIds: AsterismIds
) derives Eq:
  val unExecuted: Option[ObsIdSet] = executed.fold(editing.some)(editing.remove)
  val allAreExecuted: Boolean      = unExecuted.isEmpty

object ObsIdSetEditInfo:
  def fromObservationList(editing: ObsIdSet, observations: ObservationList): ObsIdSetEditInfo =
    val executed    = observations.executedOf(editing)
    // These are all ids with the same asterism
    val asterismIds =
      observations.get(editing.head).fold(AsterismIds.empty)(_.scienceTargetIds)
    ObsIdSetEditInfo(editing, executed, asterismIds)
