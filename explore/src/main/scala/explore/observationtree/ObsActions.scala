// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all.*
import clue.data.syntax.*
import crystal.react.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.Group
import explore.model.GroupList
import explore.model.Observation
import explore.model.ObservationList
import explore.optics.all.*
import explore.services.OdbGroupApi
import explore.services.OdbObservationApi
import explore.undo.Action
import explore.undo.AsyncAction
import japgolly.scalajs.react.*
import lucuma.core.enums.ScienceBand
import lucuma.core.model.Program
import lucuma.schemas.ObservationDB.Types.*
import lucuma.ui.optics.*
import monocle.Lens

object ObsActions:
  private val obsGroupInfo: Lens[Observation, (Option[Group.Id], NonNegShort)] =
    (Observation.groupId, Observation.groupIndex).disjointZip

  def obsGroupInfo(
    obsId:  Observation.Id
  )(using
    odbApi: OdbObservationApi[IO]
  ): Action[ObservationList, Option[(Option[Group.Id], NonNegShort)]] =
    Action(
      access = obsWithId(obsId).composeOptionLens(obsGroupInfo)
    )(
      onSet = (_, groupInfo) =>
        groupInfo
          .map: (groupId, index) =>
            odbApi
              .moveObservation(obsId, groupId, index)
              .void
          .orEmpty
    )

  private val groupParentInfo: Lens[Group, (Option[Group.Id], NonNegShort)] =
    (Group.parentId, Group.parentIndex).disjointZip

  def groupParentInfo(
    groupId: Group.Id
  )(using
    odbApi:  OdbGroupApi[IO]
  ): Action[GroupList, Option[(Option[Group.Id], NonNegShort)]] =
    Action(
      access = groupWithId(groupId).composeOptionLens(groupParentInfo)
    )(
      onSet = (_, parentInfo) =>
        parentInfo
          .map: (parentId, index) =>
            odbApi.moveGroup(groupId, parentId, index).void
          .orEmpty
    )

  def obsEditState(obsId: Observation.Id)(using
    odbApi: OdbObservationApi[IO]
  ) = Action(
    // need to also optimistically update the list of valid transistions
    access = obsWithId(obsId).composeOptionLens(Observation.unlawfulWorkflowState)
  )(
    onSet = (_, state) =>
      state
        .foldMap: st =>
          odbApi.setObservationWorkflowState(obsId, st)
  )

  def obsEditSubtitle(obsId: Observation.Id)(using
    odbApi: OdbObservationApi[IO]
  ): Action[ObservationList, Option[Option[NonEmptyString]]] = Action(
    access = obsWithId(obsId).composeOptionLens(Observation.subtitle)
  )(onSet =
    (_, subtitleOpt) =>
      odbApi.updateObservations(
        List(obsId),
        ObservationPropertiesInput(subtitle = subtitleOpt.flatten.orUnassign)
      )
  )

  def obsScienceBand(
    obsId: Observation.Id
  )(using odbApi: OdbObservationApi[IO]): Action[ObservationList, Option[ScienceBand]] =
    Action(
      access = obsWithId(obsId).composeOptionOptionLens(Observation.scienceBand)
    )(
      onSet = (_, scienceBand) =>
        odbApi.updateObservations(
          List(obsId),
          ObservationPropertiesInput(scienceBand = scienceBand.orUnassign)
        )
    )

  def groupExistence(
    groupId:  Group.Id,
    setGroup: Group.Id => Callback
  )(using
    odbApi:   OdbGroupApi[IO]
  ): Action[GroupList, Option[Group]] =
    Action(
      groupWithId(groupId)
    )(
      onSet = (_, groupOpt) =>
        groupOpt.fold {
          odbApi.deleteGroup(groupId)
        }(_ => setGroup(groupId).toAsync),
      onRestore = (_, groupOpt) =>
        groupOpt.fold {
          odbApi.deleteGroup(groupId)
        } { _ =>
          odbApi.undeleteGroup(groupId) >> setGroup(groupId).toAsync
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
    odbApi:      OdbObservationApi[IO]
  ): Action[ObservationList, List[Option[Observation]]] =
    Action(getter = obsListGetter(obsIds), setter = obsListSetter(obsIds))(
      onSet = (_, elemListOpt) =>
        elemListOpt.sequence.fold(
          odbApi.deleteObservations(obsIds) >>
            postMessage(s"Deleted ${obsIds.length} observation(s)")
        )(obsList => // Not much to do here, the observation must be created before we get here
          obsList.headOption.foldMap(obs => focusObs(obs.id).toAsync)
        ),
      onRestore = (_, elemWithIndexOpt) =>
        elemWithIndexOpt.sequence.fold(
          odbApi.deleteObservations(obsIds) >>
            postMessage(s"Deleted ${obsIds.length} observation(s)")
        )(obsList =>
          odbApi.undeleteObservations(obsIds) >>
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
    odbApi:      OdbObservationApi[IO]
  ): AsyncAction[ObservationList, List[Observation.Id], List[Option[Observation]]] =
    AsyncAction(
      asyncGet = idsToClone
        .traverse(odbApi.cloneObservation(_, newGroupId))
        .map(obsList => (obsList.map(_.id), obsList.map(_.some))),
      getter = obsListGetter,
      setter = obsListSetter,
      onSet = obsIds =>
        (_, elemListOpt) =>
          elemListOpt.sequence.fold(
            odbApi.deleteObservations(obsIds) >>
              postMessage(s"Deleted ${obsIds.length} observation(s)")
          )(obsList => obsList.headOption.foldMap(obs => focusObs(obs.id).toAsync)),
      onRestore = obsIds =>
        (_, elemListOpt) =>
          elemListOpt.sequence.fold(
            odbApi.deleteObservations(obsIds) >>
              postMessage(s"Deleted ${obsIds.length} observation(s)")
          )(obsList =>
            odbApi.undeleteObservations(obsIds) >>
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
    odbApi:        OdbObservationApi[IO]
  ): AsyncAction[ObservationList, Observation.Id, Option[Observation]] =
    AsyncAction(
      asyncGet = odbApi.createObservation(programId, parentGroupId).map(o => (o.id, o.some)),
      getter = singleObsGetter,
      setter = singleObsSetter,
      onSet = obsId =>
        (_, optObs) =>
          optObs.fold(
            odbApi.deleteObservations(List(obsId)) >>
              postMessage(s"Deleted observation $obsId")
          )(_ => focusObs(obsId).toAsync),
      onRestore = obsId =>
        (_, optObs) =>
          optObs.fold(
            odbApi.deleteObservations(List(obsId)) >>
              postMessage(s"Deleted observation $obsId")
          )(_ =>
            odbApi.undeleteObservations(List(obsId)) >>
              postMessage(s"Restored observation $obsId") >> focusObs(obsId).toAsync
          )
    )
