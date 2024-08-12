// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.react.syntax.all.*
import explore.common.AsterismQueries
import explore.common.TargetQueries
import explore.model.ObsIdSet
import explore.model.Observation
import explore.model.ObservationList
import explore.model.ObservationsAndTargets
import explore.model.OnCloneParameters
import explore.undo.*
import japgolly.scalajs.react.*
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Enums.Existence
import lucuma.schemas.model.TargetWithId

import scala.annotation.unused
import explore.model.syntax.all.isTargetInOtherObs

object TargetCloneAction {
  extension (obsAndTargets: ObservationsAndTargets)
    private def cloneTargetForObservations(
      originalId: Target.Id,
      clone:      TargetWithId,
      obsIds:     ObsIdSet
    ): ObservationsAndTargets =
      val obs = obsIds.idSet.foldLeft(obsAndTargets._1)((list, obsId) =>
        list.updatedValueWith(obsId, Observation.scienceTargetIds.modify(_ - originalId + clone.id))
      )
      val ts  = obsAndTargets._2 + (clone.id -> clone.target)
      (obs, ts)
    private def unCloneTargetForObservations(
      originalId: Target.Id,
      cloneId:    Target.Id,
      obsIds:     ObsIdSet
    ): ObservationsAndTargets =
      val obs = obsIds.idSet.foldLeft(obsAndTargets._1)((list, obsId) =>
        list.updatedValueWith(obsId, Observation.scienceTargetIds.modify(_ + originalId - cloneId))
      )
      val ts =
        // determine if the observation has been assigned to additional observations since the cloning.
        // If it has been assigned to other observations, we won't delete it locally or remotely.
        if (obsAndTargets._1.isTargetInOtherObs(cloneId, obsIds))
          obsAndTargets._2
        else
          obsAndTargets._2 - cloneId
      (obs, ts)

  private def getter(cloneId: Target.Id): ObservationsAndTargets => Option[Target] =
    _._2.get(cloneId)

  private def setter(originalId: Target.Id, clone: TargetWithId, obsIds: ObsIdSet)(
    @unused optClone: Option[Target]
  ): ObservationsAndTargets => ObservationsAndTargets = obsAndTargets =>
    // if the clone is in the targets, we're undoing.
    obsAndTargets._2
      .get(clone.id)
      .fold(
        obsAndTargets.cloneTargetForObservations(originalId, clone, obsIds)
      )(_ => obsAndTargets.unCloneTargetForObservations(originalId, clone.id, obsIds))

  private def updateRemote(
    programId:    Program.Id,
    onCloneParms: OnCloneParameters,
    observations: ObservationList
  )(using
    FetchClient[IO, ObservationDB]
  ): IO[Unit] =
    val optExistence =
      if (onCloneParms.areCreating) Existence.Present.some
      else {
        // If the clone has been assigned to another observation (unlikely), perhaps by another
        // user or in another session , then we won't delete it
        if (observations.isTargetInOtherObs(onCloneParms.cloneId, onCloneParms.obsIds))
          none
        else
          Existence.Deleted.some
      }
    optExistence.foldMap(existence =>
      TargetQueries.setTargetExistence[IO](programId, onCloneParms.cloneId, existence)
    ) >>
      AsterismQueries.addAndRemoveTargetsFromAsterisms(onCloneParms.obsIds.toList,
                                                       toAdd = List(onCloneParms.idToAdd),
                                                       toRemove = List(onCloneParms.idToRemove)
      )

  def cloneTarget(
    programId:  Program.Id,
    originalId: Target.Id,
    clone:      TargetWithId,
    obsIds:     ObsIdSet,
    onClone:    OnCloneParameters => Callback
  )(using
    FetchClient[IO, ObservationDB]
  ): Action[ObservationsAndTargets, Option[Target]] =
    Action[ObservationsAndTargets, Option[Target]](getter(clone.id),
                                                   setter(originalId, clone, obsIds)
    )(
      onSet = (_, _) => IO.unit, // clone is created and first `onClone` called outside of Action
      onRestore = (obsAndTargets, optClone) =>
        val params = OnCloneParameters(originalId, clone.id, obsIds, optClone.isDefined)
        onClone(params).toAsync >>
          updateRemote(programId, params, obsAndTargets._1)
    )
}
