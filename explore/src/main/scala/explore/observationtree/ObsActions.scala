// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import clue.data.syntax.*
import crystal.react.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.GroupQueries
import explore.model.Group
import explore.model.GroupList
import explore.model.Observation
import explore.model.ObservationList
import explore.optics.all.*
import explore.undo.Action
import explore.undo.AsyncAction
import japgolly.scalajs.react.*
import lucuma.core.enums.ScienceBand
import lucuma.core.model.Program
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import lucuma.ui.optics.*
import monocle.Lens
import queries.common.ObsQueriesGQL.*
import queries.schemas.odb.ObsQueries

object ObsActions:
  private val obsGroupInfo: Lens[Observation, (Option[Group.Id], NonNegShort)] =
    (Observation.groupId, Observation.groupIndex).disjointZip

  def obsGroupInfo(
    obsId: Observation.Id
  )(using
    FetchClient[IO, ObservationDB]
  ): Action[ObservationList, Option[(Option[Group.Id], NonNegShort)]] =
    Action(
      access = obsWithId(obsId).composeOptionLens(obsGroupInfo)
    )(
      onSet = (_, groupInfo) =>
        groupInfo
          .map: (groupId, index) =>
            ObsQueries
              .moveObservation[IO](obsId, groupId, index)
              .void
          .orEmpty
    )

  private val groupParentInfo: Lens[Group, (Option[Group.Id], NonNegShort)] =
    (Group.parentId, Group.parentIndex).disjointZip

  def groupParentInfo(
    groupId: Group.Id
  )(using
    FetchClient[IO, ObservationDB]
  ): Action[GroupList, Option[(Option[Group.Id], NonNegShort)]] =
    Action(
      access = groupWithId(groupId).composeOptionLens(groupParentInfo)
    )(
      onSet = (_, parentInfo) =>
        parentInfo
          .map: (parentId, index) =>
            GroupQueries
              .moveGroup[IO](groupId, parentId, index)
              .void
          .orEmpty
    )

  def obsEditState(obsId: Observation.Id)(using
    FetchClient[IO, ObservationDB]
  ) = Action(
    // need to also optimistically update the list of valid transistions
    access = obsWithId(obsId).composeOptionLens(Observation.unlawfulWorkflowState)
  )(
    onSet = (_, state) =>
      state
        .foldMap: st =>
          SetObservationWorkflowStateMutation[IO]
            .execute:
              SetObservationWorkflowStateInput(obsId, st)
            .raiseGraphQLErrors
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
        .raiseGraphQLErrors
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
          .raiseGraphQLErrors
          .void
    )

  def groupExistence(
    groupId:  Group.Id,
    setGroup: Group.Id => Callback
  )(using
    FetchClient[IO, ObservationDB]
  ): Action[GroupList, Option[Group]] =
    Action(
      groupWithId(groupId)
    )(
      onSet = (_, groupOpt) =>
        groupOpt.fold {
          GroupQueries.deleteGroup[IO](groupId)
        }(_ => setGroup(groupId).toAsync),
      onRestore = (_, groupOpt) =>
        groupOpt.fold {
          GroupQueries.deleteGroup[IO](groupId)
        } { _ =>
          GroupQueries.undeleteGroup[IO](groupId) >> setGroup(groupId).toAsync
        }
    )

  private def singleObsGetter(obsId: Observation.Id): ObservationList => Option[Observation] =
    _.get(obsId)

  private def obsListGetter(
    obsIds: List[Observation.Id]
  ): ObservationList => List[Option[Observation]] =
    obsList => obsIds.map(obsList.get)

  private def singleObsSetter(obsId: Observation.Id)(
    obsOpt: Option[Observation]
  ): ObservationList => ObservationList =
    obsList =>
      obsOpt.fold(obsList - obsId): obs =>
        obsList + (obsId -> obs)

  private def obsListSetter(obsIds: List[Observation.Id])(
    obsOpts: List[Option[Observation]]
  ): ObservationList => ObservationList =
    obsList =>
      obsIds.zip(obsOpts).foldLeft(obsList) { case (acc, (obsId, obsOpt)) =>
        singleObsSetter(obsId)(obsOpt)(acc)
      }

  def obsExistence(
    obsIds:      List[Observation.Id],
    focusObs:    Observation.Id => Callback = _ => Callback.empty,
    postMessage: String => IO[Unit] = _ => IO.unit
  )(using
    FetchClient[IO, ObservationDB]
  ): Action[ObservationList, List[Option[Observation]]] =
    Action(getter = obsListGetter(obsIds), setter = obsListSetter(obsIds))(
      onSet = (_, elemListOpt) =>
        elemListOpt.sequence.fold(
          ObsQueries.deleteObservations[IO](obsIds) >>
            postMessage(s"Deleted ${obsIds.length} observation(s)")
        )(obsList => // Not much to do here, the observation must be created before we get here
          obsList.headOption.foldMap(obs => focusObs(obs.id).toAsync)
        ),
      onRestore = (_, elemWithIndexOpt) =>
        elemWithIndexOpt.sequence.fold(
          ObsQueries.deleteObservations[IO](obsIds) >>
            postMessage(s"Deleted ${obsIds.length} observation(s)")
        )(obsList =>
          ObsQueries.undeleteObservations[IO](obsIds) >>
            postMessage(s"Restored ${obsIds.length} observation(s)") >>
            obsList.headOption.foldMap(obs => focusObs(obs.id).toAsync)
        )
    )

  def cloneObservations(
    idsToClone:  List[Observation.Id],
    newGroupId:  Option[Group.Id],
    focusObs:    Observation.Id => Callback = _ => Callback.empty,
    postMessage: String => IO[Unit] = _ => IO.unit
  )(using
    FetchClient[IO, ObservationDB]
  ): AsyncAction[ObservationList, List[Observation.Id], List[Option[Observation]]] =
    AsyncAction(
      asyncGet = idsToClone
        .traverse(ObsQueries.cloneObservation[IO](_, newGroupId))
        .map(obsList => (obsList.map(_.id), obsList.map(_.some))),
      getter = obsListGetter,
      setter = obsListSetter,
      onSet = obsIds =>
        (_, elemListOpt) =>
          elemListOpt.sequence.fold(
            ObsQueries.deleteObservations[IO](obsIds) >>
              postMessage(s"Deleted ${obsIds.length} observation(s)")
          )(obsList => obsList.headOption.foldMap(obs => focusObs(obs.id).toAsync)),
      onRestore = obsIds =>
        (_, elemListOpt) =>
          elemListOpt.sequence.fold(
            ObsQueries.deleteObservations[IO](obsIds) >>
              postMessage(s"Deleted ${obsIds.length} observation(s)")
          )(obsList =>
            ObsQueries.undeleteObservations[IO](obsIds) >>
              postMessage(s"Restored ${obsIds.length} observation(s)") >>
              obsList.headOption.foldMap(obs => focusObs(obs.id).toAsync)
          )
    )

  def insertObservation(
    programId:     Program.Id,
    parentGroupId: Option[Group.Id],
    focusObs:      Observation.Id => Callback = _ => Callback.empty,
    postMessage:   String => IO[Unit] = _ => IO.unit
  )(using
    FetchClient[IO, ObservationDB]
  ): AsyncAction[ObservationList, Observation.Id, Option[Observation]] =
    AsyncAction(
      asyncGet =
        ObsQueries.createObservation[IO](programId, parentGroupId).map(o => (o.id, o.some)),
      getter = singleObsGetter,
      setter = singleObsSetter,
      onSet = obsId =>
        (_, optObs) =>
          optObs.fold(
            ObsQueries.deleteObservations[IO](List(obsId)) >>
              postMessage(s"Deleted observation $obsId")
          )(_ => focusObs(obsId).toAsync),
      onRestore = obsId =>
        (_, optObs) =>
          optObs.fold(
            ObsQueries.deleteObservations[IO](List(obsId)) >>
              postMessage(s"Deleted observation $obsId")
          )(_ =>
            ObsQueries.undeleteObservations[IO](List(obsId)) >>
              postMessage(s"Restored observation $obsId") >> focusObs(obsId).toAsync
          )
    )
