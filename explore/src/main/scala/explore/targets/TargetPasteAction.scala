// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import clue.TransactionalClient
import clue.data.syntax.*
import crystal.react.View
import crystal.react.implicits.*
import explore.common.AsterismQueries
import explore.common.AsterismQueries.*
import explore.model.AsterismGroup
import explore.model.ObsIdSet
import explore.model.ObsSummaryWithConstraintsAndConf
import explore.model.TargetIdSet
import explore.model.TargetWithObs
import explore.model.syntax.all.*
import explore.undo.*
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import queries.common.TargetQueriesGQL

import scala.annotation.unused
import scala.collection.immutable.SortedSet

object TargetPasteAction {
  // This never turns out to be useful, so we'll just use Unit
  private def getter: AsterismGroupsWithObs => Unit = _ => ()

  private def setter(
    obsIds:       ObsIdSet,
    targetIds:    TargetIdSet
  )(
    @unused unit: Unit
  ): AsterismGroupsWithObs => AsterismGroupsWithObs = agwo =>
    // the groups should always exist unless something changed outside the scope of this context, in which case
    // we do nothing
    val origAsterismGroups = agwo.asterismGroups
    val oCurrentGroup      = origAsterismGroups.findContainingObsIds(obsIds)
    val tidSet             = targetIds.toSortedSet

    oCurrentGroup.fold(agwo)(currentGroup =>
      // if the current contains the target ids, that means we are undoing
      val isUndo       = tidSet.subsetOf(currentGroup.targetIds)
      val newTargetIds =
        if (isUndo) currentGroup.targetIds -- tidSet
        else currentGroup.targetIds ++ tidSet

      val newObservations = obsIds.idSet.foldLeft(agwo.observations)((acc, obsId) =>
        acc.updatedWith(obsId)(_.map(_.copy(scienceTargetIds = newTargetIds)))
      )

      val updatedTargetsWithObs = targetIds.idSet.foldLeft(agwo.targetsWithObs)((acc, tid) =>
        acc.updatedWith(tid)(
          _.map(two => if (isUndo) two.removeObsIds(obsIds) else two.addObsIds(obsIds))
        )
      )

      // if we're not changing the entire group, we need to split up the group
      val splitAsterismGroups: AsterismGroupList =
        if (currentGroup.obsIds === obsIds)
          origAsterismGroups + currentGroup
            .copy(targetIds = newTargetIds)
            .asObsKeyValue // just update
        else
          origAsterismGroups - currentGroup.obsIds + currentGroup
            .removeObsIdsUnsafe(obsIds)
            .asObsKeyValue +
            AsterismGroup(obsIds, newTargetIds).asObsKeyValue

        // if there is already a current group with the same targets, join them
      val updatedAsterismGroups =
        origAsterismGroups.findWithTargetIds(newTargetIds).fold(splitAsterismGroups) { grp =>
          splitAsterismGroups - obsIds - grp.obsIds + grp.addObsIds(obsIds).asObsKeyValue
        }

      AsterismGroupsWithObs(asterismGroups = updatedAsterismGroups,
                            targetsWithObs = updatedTargetsWithObs,
                            observations = newObservations
      )
    )

  private def modExpanded(
    obsIds:       ObsIdSet,
    targetIds:    TargetIdSet,
    currentGroup: AsterismGroup,
    agl:          AsterismGroupList,
    isUndo:       Boolean
  ): SortedSet[ObsIdSet] => SortedSet[ObsIdSet] = eids =>
    // just expand all the affected nodes
    // current will not contain the targets
    val newTargets =
      if (isUndo) currentGroup.targetIds -- targetIds.toSortedSet
      else currentGroup.targetIds ++ targetIds.toSortedSet
    // If there is already a group with these targets, we merge
    val merged     = agl
      .findWithTargetIds(newTargets)
      .fold(eids + obsIds)(mergeWith => eids + (mergeWith.obsIds ++ obsIds))
    // if we're splitting, also expand the one left behind
    currentGroup.obsIds.remove(obsIds).fold(merged)(merged + _)

  def pasteTargets(
    programId:    Program.Id,
    obsIds:       ObsIdSet,
    targetIds:    TargetIdSet,
    selectObsIds: ObsIdSet => IO[Unit],
    expandedIds:  View[SortedSet[ObsIdSet]]
  )(using
    TransactionalClient[IO, ObservationDB]
  ): Action[AsterismGroupsWithObs, Unit] =
    Action(getter = getter, setter = setter(obsIds, targetIds))(
      onSet = (agwo, _) =>
        val agl           = agwo.asterismGroups
        val oCurrentGroup = agl.findContainingObsIds(obsIds)
        oCurrentGroup.foldMap(currentGroup =>
          val isUndo = targetIds.toSortedSet.subsetOf(currentGroup.targetIds)
          expandedIds
            .mod(modExpanded(obsIds, targetIds, currentGroup, agl, isUndo))
            .to[IO] >>
            selectObsIds(obsIds) >>
            (if (isUndo)
               AsterismQueries
                 .removeTargetsFromAsterisms[IO](programId, obsIds.toList, targetIds.toList)
             else
               AsterismQueries
                 .addTargetsToAsterisms[IO](programId, obsIds.toList, targetIds.toList))
        )
    )
}
