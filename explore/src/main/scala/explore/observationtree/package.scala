// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import clue.data.syntax.*
import crystal.react.*
import eu.timepit.refined.types.numeric.NonNegInt
import explore.DefaultErrorPolicy
import explore.common.GroupQueries
import explore.data.tree.KeyedIndexedTree.Index
import explore.data.tree.Node
import explore.model.AppContext
import explore.model.Focused
import explore.model.GroupTree
import explore.model.Observation
import explore.model.ServerIndexed
import explore.model.enums.AppTab
import explore.syntax.ui.*
import explore.undo.KIListMod
import explore.undo.KITreeMod
import explore.undo.UndoSetter
import explore.utils.ToastCtx
import japgolly.scalajs.react.*
import lucuma.core.model.Group
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Program
import lucuma.core.util.NewType
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import queries.common.ObsQueriesGQL.*
import queries.schemas.odb.ObsQueries.*
import explore.model.ObservationList
import monocle.Iso
import monocle.Lens

val obsListMod = KIListMod[Observation, Observation.Id](Observation.id)

val groupTreeMod: KITreeMod[GroupTree.Value, GroupTree.Key] =
  KITreeMod[GroupTree.Value, GroupTree.Key](GroupTree.key)

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
  ctx:          AppContext[IO],
  before:       IO[Unit] = IO.unit,
  after:        IO[Unit] = IO.unit
): IO[Unit] =
  import ctx.given

  before >>
    obsIds
      .traverse(cloneObservation[IO](_, newGroupId))
      .flatMap: newObsList =>
        ObsActions
          .obsExistence(
            newObsList.map(_.id),
            focusObs = obsId => focusObs(programId, obsId.some, ctx),
            postMessage = ToastCtx[IO].showToast(_)
          )
          .set(observations): // obsList =>
            newObsList.map(_.some)
            // obsList
            //   .zip(newObsList)
            //   // Just place the new obs at the end of the group, which is where the server clones it.
            //   .map((oldObs, newObs) => (_ + (newObs))(oldObs))
            //   // .map((oldObs, newObs) => obsListMod.upsert(newObs, NonNegInt.MaxValue)(oldObs))
          .toAsync
      .guarantee(after)

private def obsWithId(obsId: Observation.Id): Lens[ObservationList, Option[Observation]] =
  Iso.id[ObservationList].at(obsId)

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
  pos:          NonNegInt,
  observations: UndoSetter[ObservationList],
  // groups:       View[GroupList],
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
        .set(observations): // obsList =>
          List(obs.some)
        .toAsync
    // obsList
    //   .zip(List(obs))
    //   // Just place the new obs at the end of the group, which is where the server clones it.
    //   .map((oldObs, newObs) => obsListMod.upsert(newObs, NonNegInt.MaxValue)(oldObs)))
    // >> groupTree.mod:
    //   _.inserted(
    //     obs.id.asLeft,
    //     Node(ServerIndexed(obs.id.asLeft, groupIndex)),
    //     Index(parentId.map(_.asRight), NonNegInt.MaxValue)
    //   )
    // ).toAsync
    .switching(adding.zoom(AddingObservation.value.asLens).async)

private def findGrouping(
  groupId: Group.Id
): GroupTree => Option[(GroupTree.Node, GroupTree.Index)] =
  _.getNodeAndIndexByKey(groupId.asRight)

def insertGroup(
  programId: Program.Id,
  parentId:  Option[Group.Id],
  groups:    UndoSetter[GroupTree],
  adding:    View[AddingObservation],
  ctx:       AppContext[IO]
): IO[Unit] =
  import ctx.given

  GroupQueries
    .createGroup[IO](programId, parentId)
    .flatMap: (group, parentIndex) =>
      ObsActions
        .groupExistence(group.id, g => focusGroup(programId, g.some, ctx))
        .set(groups):
          (Node(ServerIndexed(group.asRight, parentIndex)),
           Index(parentId.map(_.asRight), NonNegInt.MaxValue)
          ).some
        .toAsync
    .void
    .switching(adding.zoom(AddingObservation.value.asLens).async)
