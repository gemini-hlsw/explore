// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import clue.data.syntax.*
import crystal.react.View
import crystal.react.implicits.*
import explore.common.AsterismQueries
import explore.data.KeyedIndexedList
import explore.model.AsterismGroup
import explore.model.AsterismGroupList
import explore.model.ObsIdSet
import explore.model.ObsSummary
import explore.model.ObservationList
import explore.model.ProgramSummaries
import explore.model.TargetIdSet
import explore.model.TargetWithObs
import explore.model.syntax.all.*
import explore.undo.*
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import monocle.Iso
import queries.common.TargetQueriesGQL

import scala.annotation.unused
import scala.collection.immutable.SortedSet

object TargetPasteAction {
  // This never turns out to be useful, so we'll just use Unit
  private def getter: ProgramSummaries => Unit = _ => ()

  private def setter(
    obsIds:       ObsIdSet,
    targetIds:    TargetIdSet
  )(
    @unused unit: Unit
  ): ProgramSummaries => ProgramSummaries = ps =>
    // the groups should always exist unless something changed outside the scope of this context, in which case
    // we do nothing
    val origAsterismGroups = ps.asterismGroups
    val oCurrentGroup      = origAsterismGroups.findContainingObsIds(obsIds)
    val tidSet             = targetIds.toSortedSet

    // Maybe this can be simplified even further?

    oCurrentGroup.fold(ps)(currentGroup =>
      // if the current contains the target ids, that means we are undoing
      val isUndo       = tidSet.subsetOf(currentGroup.targetIds)
      val newTargetIds =
        if (isUndo) currentGroup.targetIds -- tidSet
        else currentGroup.targetIds ++ tidSet

      ProgramSummaries.observations
        .andThen(Iso.id[ObservationList].filterIndex(obsIds.idSet.contains))
        .andThen(KeyedIndexedList.value)
        .andThen(ObsSummary.scienceTargetIds)
        .replace(newTargetIds)(ps)
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
    FetchClient[IO, ?, ObservationDB]
  ): Action[ProgramSummaries, Unit] =
    Action(getter = getter, setter = setter(obsIds, targetIds))(
      onSet = (ps, _) =>
        val agl           = ps.asterismGroups
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
