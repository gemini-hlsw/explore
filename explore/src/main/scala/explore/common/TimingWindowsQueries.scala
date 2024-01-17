// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.MonadThrow
import cats.syntax.all.*
import clue.FetchClient
import clue.data.syntax.*
import crystal.react.*
import explore.DefaultErrorPolicy
import explore.model.ObsIdSet
import explore.syntax.ui.*
import explore.utils.ToastCtx
import japgolly.scalajs.react.util.Effect.Dispatch
import lucuma.core.model.TimingWindow
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import org.typelevel.log4cats.Logger
import queries.common.ObsQueriesGQL.UpdateObservationMutation

object TimingWindowsQueries:
  def viewWithRemoteMod[F[_]: MonadThrow: Dispatch](
    obsIds: ObsIdSet,
    view:   View[List[TimingWindow]]
  )(using FetchClient[F, ObservationDB], Logger[F], ToastCtx[F]): View[List[TimingWindow]] =
    view
      .withOnMod(value =>
        UpdateObservationMutation[F]
          .execute(
            UpdateObservationsInput(
              WHERE = obsIds.toList.toWhereObservation.assign,
              SET = ObservationPropertiesInput(
                timingWindows = value.map(_.toInput).assign
              )
            )
          )
          .toastErrors
          .void
          .runAsync
      )
