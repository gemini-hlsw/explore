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

  def obsExistence(obsId: Observation.Id, setObs: Observation.Id => Callback)(using
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
          setObs(obs.id).toAsync
        },
      onRestore = (_, elemWithIndexOpt) =>
        elemWithIndexOpt.fold {
          deleteObservation[IO](obsId)
        } { case (obs, _) =>
          undeleteObservation[IO](obs.id) >>
            setObs(obs.id).toAsync
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
