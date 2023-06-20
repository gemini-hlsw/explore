// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import clue.data.syntax.*
import crystal.react.implicits.*
import explore.DefaultErrorPolicy
import explore.data.KeyedIndexedList
import explore.model.AppContext
import explore.model.Focused
import explore.model.ObsSummary
import explore.model.enums.AppTab
import explore.optics.GetAdjust
import explore.optics.all.*
import explore.undo.Action
import explore.undo.KIListMod
import explore.undo.UndoContext
import japgolly.scalajs.react.*
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
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

def cloneObs(
  programId: Program.Id,
  obsId:     Observation.Id,
  pos:       Int,
  undoCtx:   UndoContext[ObservationList],
  ctx:       AppContext[IO],
  before:    IO[Unit] = IO.unit,
  after:     IO[Unit] = IO.unit
): IO[Unit] =
  import ctx.given

  before >>
    cloneObservation[IO](obsId)
      .flatMap { obs =>
        obsExistence(programId, obs.id, o => setObs(programId, o.some, ctx))
          .mod(undoCtx)(obsListMod.upsert(obs, pos))
          .to[IO]
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

def obsEditStatus(programId: Program.Id, obsId: Observation.Id)(using
  FetchClient[IO, ObservationDB]
) = Action(
  access = obsWithId(obsId).composeOptionLens(ObsSummary.status)
)(onSet =
  (_, status) =>
    UpdateObservationMutation[IO]
      .execute(
        UpdateObservationsInput(
          programId = programId,
          WHERE = obsId.toWhereObservation.assign,
          SET = ObservationPropertiesInput(status = status.orIgnore)
        )
      )
      .void
)

def obsEditSubtitle(programId: Program.Id, obsId: Observation.Id)(using
  FetchClient[IO, ObservationDB]
) = Action(
  access = obsWithId(obsId).composeOptionLens(ObsSummary.subtitle)
)(onSet =
  (_, subtitleOpt) =>
    UpdateObservationMutation[IO]
      .execute(
        UpdateObservationsInput(
          programId = programId,
          WHERE = obsId.toWhereObservation.assign,
          SET = ObservationPropertiesInput(subtitle = subtitleOpt.flatten.orUnassign)
        )
      )
      .void
)

def obsEditAttachments(
  programId:     Program.Id,
  obsId:         Observation.Id,
  attachmentIds: Set[ObsAttachment.Id]
)(using
  FetchClient[IO, ObservationDB]
) =
  UpdateObservationMutation[IO]
    .execute(
      UpdateObservationsInput(
        programId = programId,
        WHERE = obsId.toWhereObservation.assign,
        SET = ObservationPropertiesInput(obsAttachments = attachmentIds.toList.assign)
      )
    )
    .void

def obsActiveStatus(programId: Program.Id, obsId: Observation.Id)(using
  FetchClient[IO, ObservationDB]
) = Action(
  access = obsWithId(obsId).composeOptionLens(ObsSummary.activeStatus)
)(onSet =
  (_, activeStatus) =>
    UpdateObservationMutation[IO]
      .execute(
        UpdateObservationsInput(
          programId = programId,
          WHERE = obsId.toWhereObservation.assign,
          SET = ObservationPropertiesInput(activeStatus = activeStatus.orIgnore)
        )
      )
      .void
)

def obsExistence(programId: Program.Id, obsId: Observation.Id, setObs: Observation.Id => Callback)(
  using FetchClient[IO, ObservationDB]
) =
  Action(
    access = obsListMod.withKey(obsId)
  )(
    onSet = (_, elemWithIndexOpt) =>
      elemWithIndexOpt.fold {
        deleteObservation[IO](programId, obsId)
      } { case (obs, _) =>
        // Not much to do here, the observation must be created before we get here
        setObs(obs.id).to[IO]
      },
    onRestore = (_, elemWithIndexOpt) =>
      elemWithIndexOpt.fold {
        deleteObservation[IO](programId, obsId)
      } { case (obs, _) =>
        undeleteObservation[IO](programId, obs.id) >>
          setObs(obs.id).to[IO]
      }
  )
