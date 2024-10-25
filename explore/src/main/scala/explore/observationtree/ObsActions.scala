// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import clue.data.syntax.*
import crystal.react.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.DefaultErrorPolicy
import explore.common.GroupQueries
import explore.model.GroupTree
import explore.model.Observation
import explore.optics.all.*
import explore.undo.Action
import japgolly.scalajs.react.*
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsStatus
import lucuma.core.enums.ScienceBand
import lucuma.core.model.Group
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import queries.common.ObsQueriesGQL.*
import queries.schemas.odb.ObsQueries.*
import eu.timepit.refined.types.numeric.NonNegInt
import queries.schemas.odb.ObsQueries

object ObsActions:
  def obsEditStatus(
    obsId: Observation.Id
  )(using FetchClient[IO, ObservationDB]): Action[ObservationList, Option[ObsStatus]] =
    Action(
      access = obsWithId(obsId).composeOptionLens(Observation.status)
    )(onSet =
      (_, status) =>
        UpdateObservationMutation[IO]
          .execute:
            UpdateObservationsInput(
              WHERE = obsId.toWhereObservation.assign,
              SET = ObservationPropertiesInput(status = status.orIgnore)
            )
          .void
    )

  def obsEditSubtitle(obsId: Observation.Id)(using
    FetchClient[IO, ObservationDB]
  ): Action[ObservationList, Option[Option[NonEmptyString]]] = Action(
    access = obsWithId(obsId).composeOptionLens(Observation.subtitle)
  )(onSet =
    (_, subtitleOpt) =>
      UpdateObservationMutation[IO]
        .execute:
          UpdateObservationsInput(
            WHERE = obsId.toWhereObservation.assign,
            SET = ObservationPropertiesInput(subtitle = subtitleOpt.flatten.orUnassign)
          )
        .void
  )

  def obsActiveStatus(obsId: Observation.Id)(using
    FetchClient[IO, ObservationDB]
  ): Action[ObservationList, Option[ObsActiveStatus]] =
    Action(
      access = obsWithId(obsId).composeOptionLens(Observation.activeStatus)
    )(onSet =
      (_, activeStatus) =>
        UpdateObservationMutation[IO]
          .execute:
            UpdateObservationsInput(
              WHERE = obsId.toWhereObservation.assign,
              SET = ObservationPropertiesInput(activeStatus = activeStatus.orIgnore)
            )
          .void
    )

  def obsScienceBand(
    obsId: Observation.Id
  )(using FetchClient[IO, ObservationDB]): Action[ObservationList, Option[ScienceBand]] =
    Action(
      access = obsWithId(obsId).composeOptionOptionLens(Observation.scienceBand)
    )(
      onSet = (_, scienceBand) =>
        UpdateObservationMutation[IO]
          .execute:
            UpdateObservationsInput(
              WHERE = obsId.toWhereObservation.assign,
              SET = ObservationPropertiesInput(scienceBand = scienceBand.orUnassign)
            )
          .void
    )

  def obsExistence(obsId: Observation.Id, focusObs: Observation.Id => Callback)(using
    FetchClient[IO, ObservationDB]
  ): Action[ObservationList, Option[obsListMod.ElemWithIndex]] =
    Action(
      access = obsListMod.withKey(obsId)
    )(
      onSet = (_, elemWithIndexOpt) =>
        elemWithIndexOpt.fold {
          deleteObservation[IO](obsId)
        } { case (obs, _) =>
          // Not much to do here, the observation must be created before we get here
          focusObs(obs.id).toAsync
        },
      onRestore = (_, elemWithIndexOpt) =>
        elemWithIndexOpt.fold {
          deleteObservation[IO](obsId)
        } { case (obs, _) =>
          undeleteObservation[IO](obs.id) >>
            focusObs(obs.id).toAsync
        }
    )

  def groupExistence(
    groupId:  Group.Id,
    setGroup: Group.Id => Callback
  )(using
    FetchClient[IO, ObservationDB]
  ): Action[GroupTree, Option[(GroupTree.Node, GroupTree.Index)]] =
    Action(
      getter = findGrouping(groupId),
      setter = maybeGroup =>
        groupList =>
          maybeGroup match
            case None              => groupList.removed(groupId.asRight)
            case Some((node, idx)) =>
              val group = node.value
              groupList.upserted(groupId.asRight, group, idx)
    )(
      onSet = (_, node) =>
        node.fold {
          GroupQueries.deleteGroup[IO](groupId)
        } { case (_, _) => setGroup(groupId).toAsync },
      onRestore = (_, node) =>
        node.fold {
          GroupQueries.deleteGroup[IO](groupId)
        } { case (_, _) =>
          GroupQueries.undeleteGroup[IO](groupId) >> setGroup(groupId).toAsync
        }
    )

  private def singleObsGetter(
    obsId: Observation.Id
  ): ObservationList => Option[(Observation, NonNegInt)] =
    _.getValueAndIndex(obsId)

  private def obsListGetter(
    obsIds: List[Observation.Id]
  ): ObservationList => List[Option[(Observation, NonNegInt)]] =
    obsList => obsIds.map(obsId => singleObsGetter(obsId)(obsList))

  private def singleObsSetter(obsId: Observation.Id)(
    obsOpt: Option[(Observation, NonNegInt)]
  ): ObservationList => ObservationList =
    obsList =>
      obsOpt.fold(obsList.removed(obsId)): (obs, idx) =>
        obsList.inserted(obsId, obs, idx)

  private def obsListSetter(obsIds: List[Observation.Id])(
    obsOpts: List[Option[(Observation, NonNegInt)]]
  ): ObservationList => ObservationList =
    obsList =>
      obsIds.zip(obsOpts).foldLeft(obsList) { case (acc, (obsId, obsOpt)) =>
        singleObsSetter(obsId)(obsOpt)(acc)
      }

  def deleteObservations(
    obsIds:      List[Observation.Id],
    setSummary:  IO[Unit] = IO.unit,
    postMessage: String => IO[Unit] = _ => IO.unit
  )(using
    c:           FetchClient[IO, ObservationDB]
  ): Action[ObservationList, List[Option[(Observation, NonNegInt)]]] =
    Action(getter = obsListGetter(obsIds), setter = obsListSetter(obsIds))(
      onSet = (_, loObs) =>
        loObs.sequence.fold(ObsQueries.deleteObservations[IO](obsIds))(_ =>
          ObsQueries.undeleteObservations[IO](obsIds)
        ),
      onRestore = (_, loObs) =>
        loObs.sequence.fold(
          ObsQueries.deleteObservations[IO](obsIds) >> setSummary >>
            postMessage(s"Deleted ${obsIds.length} observation(s)")
        )(_ =>
          ObsQueries.undeleteObservations[IO](obsIds) >> setSummary >>
            postMessage(s"Restored ${obsIds.length} observation(s)")
        )
    )

  def obsExistence2(obsIds: List[Observation.Id])(using
    FetchClient[IO, ObservationDB]
  ): Action[ObservationList, Option[List[obsListMod.ElemWithIndex]]] =
    Action(
      getter = obsListGetter(obsIds).andThen(_.sequence),
      setter = obsListOpt => obsListSetter(obsIds)(obsListOpt.sequence)
    )(
      onSet = (_, elemWithIndexListOpt) =>
        elemWithIndexListOpt.fold {
          ObsQueries.deleteObservations[IO](obsIds)
        } { case (obs, _) =>
          IO.unit
        // Not much to do here, the observation must be created before we get here
        // focusObs(obs.id).toAsync
        },
      // ,
      onRestore = (_, elemWithIndexOpt) => // IO.unit
        elemWithIndexOpt.fold {
          ObsQueries.deleteObservations[IO](obsIds)
        } { case (obs, _) =>
          ObsQueries.undeleteObservations[IO](obsIds)
        // undeleteObservation[IO](obs.id) >>
        //   focusObs(obs.id).toAsync
        }
    )
