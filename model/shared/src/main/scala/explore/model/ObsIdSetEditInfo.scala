// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.Order.*
import cats.derived.*
import cats.syntax.all.*
import explore.model.syntax.all.*

import scala.collection.immutable.SortedSet

case class ObsIdSetEditInfo(
  editing:   ObsIdSet,
  ongoing:   Option[ObsIdSet],
  completed: Option[ObsIdSet]
) derives Eq:
  val hasOngoingButNotCompleted: Boolean = ongoing.isDefined && completed.isEmpty

  val hasCompleted: Boolean         = completed.isDefined
  val unCompleted: Option[ObsIdSet] = completed.fold(editing.some)(editing.remove)
  val allAreCompleted: Boolean      = unCompleted.isEmpty

  // many places only care about executed or not, not ongoing vs completed
  val executed                     = ongoing.fold(completed)(o =>
    ObsIdSet.fromSortedSet(
      o.idSet.toSortedSet ++ completed.fold(SortedSet.empty[Observation.Id])(_.idSet.toSortedSet)
    )
  )
  val hasExecuted: Boolean         = executed.isDefined
  val unExecuted: Option[ObsIdSet] = executed.fold(editing.some)(editing.remove)
  val allAreExecuted: Boolean      = unExecuted.isEmpty

object ObsIdSetEditInfo:
  def fromObservationList(editing: ObsIdSet, observations: ObservationList): ObsIdSetEditInfo =
    val ongoing   = observations.ongoingOf(editing)
    val completed = observations.completedOf(editing)
    ObsIdSetEditInfo(editing, ongoing, completed)

  def of(obs: Observation): ObsIdSetEditInfo =
    val ongoing   = if (obs.isOngoing) ObsIdSet.of(obs.id).some else none
    val completed = if (obs.isCompleted) ObsIdSet.of(obs.id).some else none

    ObsIdSetEditInfo(ObsIdSet.of(obs.id), ongoing, completed)
