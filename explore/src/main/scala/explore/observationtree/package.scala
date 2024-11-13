// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import clue.data.syntax.*
import crystal.react.*
import explore.DefaultErrorPolicy
import explore.common.GroupQueries
import explore.model.AppContext
import explore.model.Focused
import explore.model.Group
import explore.model.GroupList
import explore.model.Observation
import explore.model.ObservationList
import explore.model.enums.AppTab
import explore.syntax.ui.*
import explore.undo.UndoSetter
import explore.utils.ToastCtx
import japgolly.scalajs.react.*
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Program
import lucuma.core.util.NewType
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import monocle.Iso
import monocle.Lens
import queries.common.ObsQueriesGQL.*
import queries.schemas.odb.ObsQueries.*

def focusObs[F[_]](
  programId: Program.Id,
  obsId:     Option[Observation.Id],
  ctx:       AppContext[F]
): Callback =
  ctx.pushPage(AppTab.Observations, programId, obsId.fold(Focused.None)(Focused.singleObs(_)))

def focusGroup[F[_]](
  programId: Program.Id,
  groupId:   Option[Group.Id],
  ctx:       AppContext[F]
): Callback =
  ctx.pushPage(AppTab.Observations, programId, groupId.fold(Focused.None)(Focused.group(_)))

def cloneObs(
  programId:    Program.Id,
  obsIds:       List[Observation.Id],
  newGroupId:   Option[Group.Id],
  observations: UndoSetter[ObservationList],
  ctx:          AppContext[IO]
): IO[Unit] =
  import ctx.given

  obsIds
    .traverse(cloneObservation[IO](_, newGroupId))
    .flatMap: newObsList =>
      ObsActions
        .obsExistence(
          newObsList.map(_.id),
          focusObs = obsId => focusObs(programId, obsId.some, ctx),
          postMessage = ToastCtx[IO].showToast(_)
        )
        .set(observations):
          newObsList.map(_.some)
        .toAsync

private def obsWithId(obsId: Observation.Id): Lens[ObservationList, Option[Observation]] =
  Iso.id[ObservationList].at(obsId)

private def groupWithId(groupId: Group.Id): Lens[GroupList, Option[Group]] =
  Iso.id[GroupList].at(groupId)

def obsEditAttachments(
  obsId:         Observation.Id,
  attachmentIds: Set[ObsAttachment.Id]
)(using
  FetchClient[IO, ObservationDB]
): IO[Unit] =
  UpdateObservationMutation[IO]
    .execute:
      UpdateObservationsInput(
        WHERE = obsId.toWhereObservation.assign,
        SET = ObservationPropertiesInput(obsAttachments = attachmentIds.toList.assign)
      )
    .void

object AddingObservation extends NewType[Boolean]
type AddingObservation = AddingObservation.Type

def insertObs(
  programId:    Program.Id,
  parentId:     Option[Group.Id],
  observations: UndoSetter[ObservationList],
  adding:       View[AddingObservation],
  ctx:          AppContext[IO]
): IO[Unit] =
  import ctx.given

  createObservation[IO](programId, parentId)
    .flatMap: obs =>
      ObsActions
        .obsExistence(
          List(obs.id),
          focusObs = obsId => focusObs(programId, obsId.some, ctx),
          postMessage = ToastCtx[IO].showToast(_)
        )
        .set(observations):
          List(obs.some)
        .toAsync
    .switching(adding.zoom(AddingObservation.value.asLens).async)

def insertGroup(
  programId: Program.Id,
  parentId:  Option[Group.Id],
  groups:    UndoSetter[GroupList],
  adding:    View[AddingObservation],
  ctx:       AppContext[IO]
): IO[Unit] =
  import ctx.given

  GroupQueries
    .createGroup[IO](programId, parentId)
    .flatMap: group =>
      ObsActions
        .groupExistence(group.id, g => focusGroup(programId, g.some, ctx))
        .set(groups)(group.some)
        .toAsync
    .void
    .switching(adding.zoom(AddingObservation.value.asLens).async)
