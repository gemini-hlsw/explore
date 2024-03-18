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
import explore.data.KeyedIndexedList
import explore.model.AppContext
import explore.model.Focused
import explore.model.GroupElement
import explore.model.GroupList
import explore.model.Grouping
import explore.model.ObsSummary
import explore.model.enums.AppTab
import explore.optics.GetAdjust
import explore.optics.all.*
import explore.syntax.ui.*
import explore.undo.Action
import explore.undo.KIListMod
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import lucuma.core.model.Group
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.util.NewType
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import monocle.Focus
import queries.common.ObsQueriesGQL.*
import queries.schemas.odb.ObsQueries.*

val obsListMod = KIListMod[ObsSummary, Observation.Id](ObsSummary.id)

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
  pos:          Int,
  observations: UndoSetter[ObservationList],
  ctx:          AppContext[IO],
  before:       IO[Unit] = IO.unit,
  after:        IO[Unit] = IO.unit
): IO[Unit] =
  import ctx.given

  before >>
    cloneObservation[IO](obsId)
      .flatMap { obs =>
        obsExistence(obs.id, o => setObs(programId, o.some, ctx))
          .mod(observations)(obsListMod.upsert(obs, pos))
          .toAsync
      }
      .guarantee(after)

private def obsWithId(
  obsId: Observation.Id
): GetAdjust[KeyedIndexedList[Observation.Id, ObsSummary], Option[
  ObsSummary
]] =
  obsListMod
    .withKey(obsId)
    .composeOptionLens(Focus[(ObsSummary, Int)](_._1))

def obsEditStatus(obsId: Observation.Id)(using
  FetchClient[IO, ObservationDB]
) = Action(
  access = obsWithId(obsId).composeOptionLens(ObsSummary.status)
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
  access = obsWithId(obsId).composeOptionLens(ObsSummary.subtitle)
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
  access = obsWithId(obsId).composeOptionLens(ObsSummary.activeStatus)
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
  pos:          Int,
  observations: UndoSetter[ObservationList],
  adding:       View[AddingObservation],
  ctx:          AppContext[IO]
): IO[Unit] =
  import ctx.given

  createObservation[IO](programId)
    .flatMap { obs =>
      obsExistence(obs.id, o => setObs(programId, o.some, ctx))
        .mod(observations)(obsListMod.upsert(obs, pos))
        .toAsync
    }
    .switching(adding.zoom(AddingObservation.value.asLens).async)

private def findGrouping(groupId: Group.Id): GroupList => Option[Grouping] = _.collectFirstSome(
  _.value.toOption.filter(_.id === groupId)
)

def groupExistence(groupId: Group.Id): Action[GroupList, Option[Grouping]] = Action(
  getter = findGrouping(groupId),
  setter = group =>
    groupList =>
      val groupEl =
        group.map(grouping => GroupElement(grouping.asRight, grouping.parentId))
      groupList ++ groupEl
)(
  // TODO: handle undo by removing (and undoing remove) of groups
  onSet = (_, _) => IO.unit
)

def insertGroup(
  programId: Program.Id,
  groups:    UndoSetter[GroupList],
  adding:    View[AddingObservation],
  ctx:       AppContext[IO]
): IO[Unit] =
  import ctx.given
  GroupQueries
    .createGroup[IO](programId)
    .flatMap(group => groupExistence(group.id).set(groups)(group.some).toAsync)
    .switching(adding.zoom(AddingObservation.value.asLens).async)
