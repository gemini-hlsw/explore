// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import clue.FetchClient
import crystal.react.syntax.all.*
import explore.common.AsterismQueries
import explore.model.ObsIdSet
import explore.model.ObservationsAndTargets
import explore.model.OnAsterismUpdateParams
import explore.model.syntax.all.*
import explore.undo.*
import japgolly.scalajs.react.*
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.TargetWithId

import scala.annotation.unused

object AsterismActions:
  extension (obsAndTargets: ObservationsAndTargets)
    private def asterismHasTarget(targetId: Target.Id, obsIds: ObsIdSet): Boolean =
      // since we're dealing with asterisms, if the target is in one observation in obsIds, it should be in all
      obsAndTargets._1.get(obsIds.head).fold(false)(_.scienceTargetIds.contains(targetId))
    // If the target was created, but has been assigned to another observation (unlikely), perhaps by another
    // user or in another session, then we won't delete it
    private def shouldDelete(targetId: Target.Id, obsIds: ObsIdSet, createdTarget: Boolean) =
      createdTarget && !obsAndTargets._1.isTargetInOtherObs(targetId, obsIds)

  // returns True if the observations contain the target
  private def getter(targetId: Target.Id, obsIds: ObsIdSet): ObservationsAndTargets => Boolean =
    _.asterismHasTarget(targetId, obsIds)

  private def setter(target: TargetWithId, obsIds: ObsIdSet, createdTarget: Boolean)(
    @unused unused: Boolean
  ): ObservationsAndTargets => ObservationsAndTargets = obsAndTargets =>
    if (obsAndTargets.asterismHasTarget(target.id, obsIds))
      // we're removing the target from the asterisms
      val obs     = obsAndTargets._1.removeTargetFromObservations(target.id, obsIds)
      val targets =
        if (obsAndTargets.shouldDelete(target.id, obsIds, createdTarget))
          obsAndTargets._2 - target.id
        else obsAndTargets._2
      (obs, targets)
    else
      val obs     = obsAndTargets._1.addTargetToObservations(target.id, obsIds)
      val targets =
        if (createdTarget) obsAndTargets._2 + (target.id -> target.target)
        else obsAndTargets._2
      (obs, targets)

  private def updateRemote(
    targetId:      Target.Id,
    obsIds:        ObsIdSet,
    obsAndTargets: ObservationsAndTargets, // this is the value before the setter
    createdTarget: Boolean
  )(using
    FetchClient[IO, ObservationDB]
  ): IO[Unit] =
    val asterismHasTarget = obsAndTargets.asterismHasTarget(targetId, obsIds)
    val targetList        = List(targetId)
    val obsList           = obsIds.toList

    // if we change the existence, we group the target and asterism updates
    if (asterismHasTarget)
      // Removing it
      if (obsAndTargets.shouldDelete(targetId, obsIds, createdTarget))
        AsterismQueries.deleteTargetsAndRemoveFromAsterism(obsList, targetList)
      else AsterismQueries.removeTargetsFromAsterisms(obsList, targetList)
    else if (createdTarget)
      AsterismQueries.undeleteTargetsAndAddToAsterism(obsList, targetList)
    else AsterismQueries.addTargetsToAsterisms(obsList, targetList)

  def addTargetToAsterisms(
    target:           TargetWithId,
    obsIds:           ObsIdSet,
    createdTarget:    Boolean,
    onAsterismUpdate: OnAsterismUpdateParams => Callback
  )(using
    FetchClient[IO, ObservationDB]
  ): Action[ObservationsAndTargets, Boolean] =
    Action(getter(target.id, obsIds), setter(target, obsIds, createdTarget))(
      onSet = (obsAndTargets, _) => updateRemote(target.id, obsIds, obsAndTargets, createdTarget),
      onRestore =
        (obsAndTargets, hasTarget) => // the pre-setter obsAndTargets and postSetter hasTarget
          onAsterismUpdate(OnAsterismUpdateParams(target.id, obsIds, true, hasTarget)).toAsync >>
            updateRemote(target.id, obsIds, obsAndTargets, createdTarget)
    )

  def removeTargetFromAsterisms(
    target:           TargetWithId,
    obsIds:           ObsIdSet,
    onAsterismUpdate: OnAsterismUpdateParams => Callback
  )(using
    FetchClient[IO, ObservationDB]
  ): Action[ObservationsAndTargets, Boolean] =
    Action(getter(target.id, obsIds), setter(target, obsIds, false))(
      onSet = (
        obsAndTargets,
        _
      ) => updateRemote(target.id, obsIds, obsAndTargets, false),
      onRestore =
        (obsAndTargets, hasTarget) => // the pre-setter obsAndTargets and postSetter hasTarget
          onAsterismUpdate(OnAsterismUpdateParams(target.id, obsIds, false, hasTarget)).toAsync >>
            updateRemote(target.id, obsIds, obsAndTargets, false)
    )
