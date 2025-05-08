// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all.*
import clue.data.syntax.*
import crystal.react.*
import explore.common.GroupQueries
import explore.model.AppContext
import explore.model.Attachment
import explore.model.Focused
import explore.model.Group
import explore.model.GroupList
import explore.model.Observation
import explore.model.ObservationList
import explore.model.enums.AppTab
import explore.services.OdbObservationApi
import explore.syntax.ui.*
import explore.undo.UndoSetter
import explore.utils.*
import japgolly.scalajs.react.*
import lucuma.core.model.Program
import lucuma.core.util.NewBoolean
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import monocle.Iso
import monocle.Lens

def focusObs[F[_]](
  programId: Program.Id,
  obsId:     Option[Observation.Id],
  ctx:       AppContext[F]
): Callback =
  ctx.pushPage:
    (AppTab.Observations, programId, obsId.fold(Focused.None)(Focused.singleObs(_))).some

def focusGroup[F[_]](
  programId: Program.Id,
  groupId:   Option[Group.Id],
  ctx:       AppContext[F]
): Callback =
  ctx.pushPage:
    (AppTab.Observations, programId, groupId.fold(Focused.None)(Focused.group(_))).some

def cloneObs(
  programId:    Program.Id,
  obsIds:       List[Observation.Id],
  newGroupId:   Option[Group.Id],
  observations: UndoSetter[ObservationList],
  ctx:          AppContext[IO]
): IO[Unit] =
  import ctx.given

  ObsActions
    .cloneObservations(
      obsIds,
      newGroupId,
      focusObs = obsId => focusObs(programId, obsId.some, ctx),
      postMessage = ToastCtx[IO].showToast(_)
    )(observations)
    .void

private def obsWithId(obsId: Observation.Id): Lens[ObservationList, Option[Observation]] =
  Iso.id[ObservationList].at(obsId)

private def groupWithId(groupId: Group.Id): Lens[GroupList, Option[Group]] =
  Iso.id[GroupList].at(groupId)

def obsEditAttachments(
  obsId:         Observation.Id,
  attachmentIds: Set[Attachment.Id]
)(using
  odbApi:        OdbObservationApi[IO]
): IO[Unit] =
  odbApi.updateObservations(
    List(obsId),
    ObservationPropertiesInput(attachments = attachmentIds.toList.assign)
  )

object AddingObservation extends NewBoolean
type AddingObservation = AddingObservation.Type

def insertObs(
  programId:    Program.Id,
  parentId:     Option[Group.Id],
  observations: UndoSetter[ObservationList],
  adding:       View[AddingObservation],
  ctx:          AppContext[IO]
): IO[Unit] =
  import ctx.given

  ObsActions
    .insertObservation(
      programId,
      parentId,
      focusObs = obsId => focusObs(programId, obsId.some, ctx),
      postMessage = ToastCtx[IO].showToast(_)
    )(observations)
    .void
    .switching(adding.as(AddingObservation.Value).async)
    .withToastDuring("Creating observation")

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
    .switching(adding.as(AddingObservation.Value).async)
