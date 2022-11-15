// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import clue.TransactionalClient
import clue.data.syntax.*
import crystal.react.implicits.*
import explore.data.KeyedIndexedList
import explore.model.ObsSummaryWithTitleConstraintsAndConf
import explore.optics.GetAdjust
import explore.optics.all.*
import explore.undo.Action
import explore.undo.KIListMod
import japgolly.scalajs.react.Callback
import lucuma.core.model.Observation
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import monocle.Focus
import monocle.Iso
import queries.common.ObsQueriesGQL.*
import queries.schemas.odb.ODBConversions.*
import queries.schemas.odb.ObsQueries
import lucuma.core.model.Program

trait ObsListActions {
  protected val obsListMod =
    KIListMod[ObsSummaryWithTitleConstraintsAndConf, Observation.Id](
      ObsSummaryWithTitleConstraintsAndConf.id
    )

  private def obsWithId(
    obsId: Observation.Id
  ): GetAdjust[KeyedIndexedList[Observation.Id, ObsSummaryWithTitleConstraintsAndConf], Option[
    ObsSummaryWithTitleConstraintsAndConf
  ]] =
    obsListMod
      .withKey(obsId)
      .composeOptionLens(Focus[(ObsSummaryWithTitleConstraintsAndConf, Int)](_._1))

  def obsEditStatus(programId: Program.Id, obsId: Observation.Id)(using
    TransactionalClient[IO, ObservationDB]
  ) = Action(
    access = obsWithId(obsId).composeOptionLens(ObsSummaryWithTitleConstraintsAndConf.status)
  )(onSet =
    (_, status) =>
      UpdateObservationMutation
        .execute[IO](
          UpdateObservationsInput(
            programId = programId,
            WHERE = obsId.toWhereObservation.assign,
            SET = ObservationPropertiesInput(status = status.orIgnore)
          )
        )
        .void
  )

  def obsEditSubtitle(programId: Program.Id, obsId: Observation.Id)(using
    TransactionalClient[IO, ObservationDB]
  ) = Action(
    access = obsWithId(obsId).composeOptionLens(ObsSummaryWithTitleConstraintsAndConf.subtitle)
  )(onSet =
    (_, subtitleOpt) =>
      UpdateObservationMutation
        .execute[IO](
          UpdateObservationsInput(
            programId = programId,
            WHERE = obsId.toWhereObservation.assign,
            SET = ObservationPropertiesInput(subtitle = subtitleOpt.flatten.orUnassign)
          )
        )
        .void
  )

  def obsActiveStatus(programId: Program.Id, obsId: Observation.Id)(using
    TransactionalClient[IO, ObservationDB]
  ) = Action(
    access = obsWithId(obsId).composeOptionLens(ObsSummaryWithTitleConstraintsAndConf.activeStatus)
  )(onSet =
    (_, activeStatus) =>
      UpdateObservationMutation
        .execute[IO](
          UpdateObservationsInput(
            programId = programId,
            WHERE = obsId.toWhereObservation.assign,
            SET = ObservationPropertiesInput(activeStatus = activeStatus.orIgnore)
          )
        )
        .void
  )

  def obsExistence(obsId: Observation.Id, setObs: Observation.Id => Callback)(using
    TransactionalClient[IO, ObservationDB]
  ) =
    Action(
      access = obsListMod.withKey(obsId)
    )(
      onSet = (_, elemWithIndexOpt) =>
        elemWithIndexOpt.fold {
          ObsQueries.deleteObservation[IO](obsId)
        } { case (obs, _) =>
          // Not much to do here, the observation must be created before we get here
          setObs(obs.id).to[IO]
        },
      onRestore = (_, elemWithIndexOpt) =>
        elemWithIndexOpt.fold {
          ObsQueries.deleteObservation[IO](obsId)
        } { case (obs, _) =>
          ObsQueries.undeleteObservation[IO](obs.id) >>
            setObs(obs.id).to[IO]
        }
    )
}

object ObsListActions extends ObsListActions
