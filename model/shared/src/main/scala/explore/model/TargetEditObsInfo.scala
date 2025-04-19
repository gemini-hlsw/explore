// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import explore.model.syntax.all.*
import lucuma.core.model.Target

/**
 * Information about the observations associated with a target that is being edited
 *
 * @param current
 *   The observations for with the target is currently being edited.
 * @param allForTarget
 *   All of the observations associated with a given target.
 * @param executedForTarget
 *   The executed observations associated with the target.
 */
case class TargetEditObsInfo(
  current:           Option[ObsIdSet],
  allForTarget:      Option[ObsIdSet],
  executedForTarget: Option[ObsIdSet]
) derives Eq:
  private def unexecuted(fullSet: Option[ObsIdSet]): Option[ObsIdSet] =
    fullSet.flatMap(b => executedForTarget.fold(b.some)(b -- _))

  lazy val isReadonly: Boolean                  = current.fold(allForTargetAreExecuted)(_ => allCurrentAreExecuted)
  // the `other*` values only really have meaning if editing is not empty, and if we are editing, we should also have allForTarget
  lazy val otherObs: Option[ObsIdSet]           = (current, allForTarget).flatMapN((e, a) => a -- e)
  lazy val otherObsCount: Long                  = otherObs.fold(0L)(_.size)
  lazy val otherUnexecutedObs: Option[ObsIdSet] = unexecuted(otherObs)
  lazy val otherUnexecutedObsCount: Long        = otherUnexecutedObs.fold(0L)(_.size)

  lazy val unexecutedForCurrent: Option[ObsIdSet] = unexecuted(current)
  lazy val allCurrentAreExecuted: Boolean         = current.isDefined && unexecutedForCurrent.isEmpty
  lazy val allCurrentAreOK: Boolean               =
    current.fold(true)(ids =>
      executedForTarget.fold(true)(ex => ids.idSet.intersect(ex.idSet).isEmpty)
    )

  lazy val unexecutedForTarget: Option[ObsIdSet] = unexecuted(allForTarget)
  lazy val allForTargetAreExecuted: Boolean      = allForTarget.isDefined && unexecutedForTarget.isEmpty
  lazy val allForTargetAreOK: Boolean            = executedForTarget.isEmpty

object TargetEditObsInfo:
  def fromProgramSummaries(
    tid:       Target.Id,
    current:   Option[ObsIdSet],
    summaries: ProgramSummaries
  ): TargetEditObsInfo =
    val allObsIds = summaries.targetsWithObs.get(tid).map(_.obsIds).flatMap(ObsIdSet.fromSortedSet)
    // we should always find the ids in `observations`
    val executed  = allObsIds.flatMap(summaries.observations.executedOf)
    TargetEditObsInfo(
      current,
      allObsIds,
      executed
    )
