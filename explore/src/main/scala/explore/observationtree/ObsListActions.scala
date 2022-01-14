// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all._
import clue.TransactionalClient
import clue.data.syntax._
import crystal.react.View
import crystal.react.implicits._
import explore.common.ObsQueriesGQL._
import explore.data.KeyedIndexedList
import explore.implicits._
import explore.model.FocusedObs
import explore.model.ObsSummaryWithTargetsAndConstraints
import explore.optics.GetAdjust
import explore.undo.Action
import explore.undo.KIListMod
import lucuma.core.model.Observation
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types._
import monocle.Focus

object ObsListActions {
  protected val obsListMod =
    KIListMod[ObsSummaryWithTargetsAndConstraints, Observation.Id](
      ObsSummaryWithTargetsAndConstraints.id
    )

  private def obsWithId(
    obsId: Observation.Id
  ): GetAdjust[KeyedIndexedList[Observation.Id, ObsSummaryWithTargetsAndConstraints], Option[
    ObsSummaryWithTargetsAndConstraints
  ]] =
    obsListMod
      .withKey(obsId)
      .composeOptionLens(Focus[(ObsSummaryWithTargetsAndConstraints, Int)](_._1))

  def obsStatus(obsId: Observation.Id)(implicit
    c:                 TransactionalClient[IO, ObservationDB]
  ) = Action(
    access = obsWithId(obsId).composeOptionLens(ObsSummaryWithTargetsAndConstraints.status)
  )(onSet =
    (_, status) =>
      UpdateObservationMutation
        .execute[IO](
          EditObservationInput(observationId = obsId, status = status.orIgnore)
        )
        .void
  )

  def obsActiveStatus(obsId: Observation.Id)(implicit
    c:                       TransactionalClient[IO, ObservationDB]
  ) = Action(
    access = obsWithId(obsId).composeOptionLens(ObsSummaryWithTargetsAndConstraints.activeStatus)
  )(onSet =
    (_, activeStatus) =>
      UpdateObservationMutation
        .execute[IO](
          EditObservationInput(observationId = obsId, activeStatus = activeStatus.orIgnore)
        )
        .void
  )

  def obsExistence(obsId: Observation.Id, focusedObs: View[Option[FocusedObs]])(implicit
    c:                    TransactionalClient[IO, ObservationDB]
  ) =
    Action(
      access = obsListMod.withKey(obsId)
    )(
      onSet = (_, elemWithIndexOpt) =>
        elemWithIndexOpt.fold {
          ProgramDeleteObservation.execute[IO](obsId).void
        } { case (obs, _) =>
          ProgramCreateObservation
            .execute[IO](CreateObservationInput(programId = "p-2", observationId = obs.id.assign))
            .void >>
            focusedObs.set(FocusedObs(obs.id).some).to[IO]
        },
      onRestore = (_, elemWithIndexOpt) =>
        elemWithIndexOpt.fold {
          ProgramDeleteObservation.execute[IO](obsId).void
        } { case (obs, _) =>
          ProgramUndeleteObservation.execute[IO](obs.id).void >>
            focusedObs.set(FocusedObs(obs.id).some).to[IO]
        }
    )
}
