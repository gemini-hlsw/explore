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
import explore.data.KeyedIndexedList
import explore.data.tree.KeyedIndexedTree.Index
import explore.data.tree.Node
import explore.model.AppContext
import explore.model.Focused
import explore.model.GroupTree
import explore.model.Observation
import explore.model.ServerIndexed
import explore.model.enums.AppTab
import explore.optics.GetAdjust
import explore.optics.all.*
import explore.syntax.ui.*
import explore.undo.Action
import explore.undo.KIListMod
import explore.undo.KITreeMod
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import lucuma.core.model.Group
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Program
import lucuma.core.util.NewType
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import monocle.Focus
import queries.common.ObsQueriesGQL.*
import queries.schemas.odb.ObsQueries.*

val obsListMod = KIListMod[Observation, Observation.Id](Observation.id)

val groupTreeMod: KITreeMod[GroupTree.Value, GroupTree.Key] =
  KITreeMod[GroupTree.Value, GroupTree.Key](GroupTree.key)

def setObs[F[_]](
  programId: Program.Id,
  obsId:     Option[Observation.Id],
  ctx:       AppContext[F]
): Callback =
  ctx.pushPage(AppTab.Observations, programId, obsId.fold(Focused.None)(Focused.singleObs(_)))

def setGroup[F[_]](
  programId: Program.Id,
  groupId:   Option[Group.Id],
  ctx:       AppContext[F]
): Callback =
  ctx.pushPage(AppTab.Observations, programId, groupId.fold(Focused.None)(Focused.group(_)))

def cloneObs(
  programId:    Program.Id,
  obsId:        Observation.Id,
  newGroupId:   Option[Group.Id],
  observations: UndoSetter[ObservationList],
  ctx:          AppContext[IO],
  before:       IO[Unit] = IO.unit,
  after:        IO[Unit] = IO.unit
): IO[Unit] =
  import ctx.given

  before >>
    cloneObservation[IO](obsId, newGroupId)
      .flatMap: newObs =>
        obsExistence(newObs.id, o => setObs(programId, o.some, ctx))
          .mod(observations):
            // Just place the new obs at the end of the group, which is where the server clones it.
            obsListMod.upsert(newObs, NonNegInt.MaxValue)
          .toAsync
      .guarantee(after)

private def obsWithId(
  obsId: Observation.Id
): GetAdjust[KeyedIndexedList[Observation.Id, Observation], Option[
  Observation
]] =
  obsListMod
    .withKey(obsId)
    .composeOptionLens(Focus[(Observation, NonNegInt)](_._1))

def obsEditStatus(obsId: Observation.Id)(using
  FetchClient[IO, ObservationDB]
) = Action(
  access = obsWithId(obsId).composeOptionLens(Observation.status)
)(onSet =
  (_, status) =>
    UpdateObservationMutation[IO]
      .execute(
        UpdateObservationsInput(
          WHERE = obsId.toWhereObservation.assign,
          SET = ObservationPropertiesInput(status = status.orIgnore)
        )
      )
      .void
)

def obsEditSubtitle(obsId: Observation.Id)(using
  FetchClient[IO, ObservationDB]
) = Action(
  access = obsWithId(obsId).composeOptionLens(Observation.subtitle)
)(onSet =
  (_, subtitleOpt) =>
    UpdateObservationMutation[IO]
      .execute(
        UpdateObservationsInput(
          WHERE = obsId.toWhereObservation.assign,
          SET = ObservationPropertiesInput(subtitle = subtitleOpt.flatten.orUnassign)
        )
      )
      .void
)
def obsEditAttachments(
  obsId:         Observation.Id,
  attachmentIds: Set[ObsAttachment.Id]
)(using
  FetchClient[IO, ObservationDB]
) =
  UpdateObservationMutation[IO]
    .execute(
      UpdateObservationsInput(
        WHERE = obsId.toWhereObservation.assign,
        SET = ObservationPropertiesInput(obsAttachments = attachmentIds.toList.assign)
      )
    )
    .void

def obsActiveStatus(obsId: Observation.Id)(using
  FetchClient[IO, ObservationDB]
) = Action(
  access = obsWithId(obsId).composeOptionLens(Observation.activeStatus)
)(onSet =
  (_, activeStatus) =>
    UpdateObservationMutation[IO]
      .execute(
        UpdateObservationsInput(
          WHERE = obsId.toWhereObservation.assign,
          SET = ObservationPropertiesInput(activeStatus = activeStatus.orIgnore)
        )
      )
      .void
)

def obsScienceBand(obsId: Observation.Id)(using FetchClient[IO, ObservationDB]) =
  Action(
    access = obsWithId(obsId).composeOptionOptionLens(Observation.scienceBand)
  )(
    onSet = (_, scienceBand) =>
      UpdateObservationMutation[IO]
        .execute(
          UpdateObservationsInput(
            WHERE = obsId.toWhereObservation.assign,
            SET = ObservationPropertiesInput(scienceBand = scienceBand.orUnassign)
          )
        )
        .void
  )

def obsExistence(obsId: Observation.Id, setObs: Observation.Id => Callback)(using
  FetchClient[IO, ObservationDB]
) =
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

object AddingObservation extends NewType[Boolean]
type AddingObservation = AddingObservation.Type

def insertObs(
  programId:    Program.Id,
  parentId:     Option[Group.Id],
  pos:          NonNegInt,
  observations: UndoSetter[ObservationList],
  groupTree:    View[GroupTree],
  adding:       View[AddingObservation],
  ctx:          AppContext[IO]
): IO[Unit] =
  import ctx.given

  createObservation[IO](programId, parentId)
    .flatMap: (obs, groupIndex) =>
      (obsExistence(obs.id, o => setObs(programId, o.some, ctx))
        .mod(observations)(obsListMod.upsert(obs, pos)) >>
        groupTree.mod:
          _.inserted(
            obs.id.asLeft,
            Node(ServerIndexed(obs.id.asLeft, groupIndex)),
            Index(parentId.map(_.asRight), NonNegInt.MaxValue)
          )
      ).toAsync
    .switching(adding.zoom(AddingObservation.value.asLens).async)

private def findGrouping(
  groupId: Group.Id
): GroupTree => Option[(GroupTree.Node, GroupTree.Index)] =
  _.getNodeAndIndexByKey(groupId.asRight)

def groupExistence(
  groupId:  Group.Id,
  setGroup: Group.Id => Callback
)(using
  FetchClient[IO, ObservationDB]
): Action[GroupTree, Option[(GroupTree.Node, GroupTree.Index)]] = Action(
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
      groupExistence(group.id, g => setGroup(programId, g.some, ctx))
        .set(groups):
          (Node(ServerIndexed(group.asRight, parentIndex)),
           Index(parentId.map(_.asRight), NonNegInt.MaxValue)
          ).some
        .toAsync
    .void
    .switching(adding.zoom(AddingObservation.value.asLens).async)
