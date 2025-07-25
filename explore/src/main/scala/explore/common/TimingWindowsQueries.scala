// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.MonadThrow
import cats.syntax.all.*
import clue.data.syntax.*
import crystal.react.*
import explore.model.ObsIdSet
import explore.services.OdbObservationApi
import explore.syntax.ui.*
import explore.utils.ToastCtx
import japgolly.scalajs.react.util.Effect.Dispatch
import lucuma.core.model.TimingWindow
import lucuma.react.primereact.Message
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import org.typelevel.log4cats.Logger

object TimingWindowsQueries:

  def viewWithRemoteMod[F[_]: MonadThrow: Dispatch](
    obsIds: ObsIdSet,
    view:   View[List[TimingWindow]]
  )(using
    odbApi: OdbObservationApi[F]
  )(using Logger[F], ToastCtx[F]): View[List[TimingWindow]] =
    view
      .withOnMod: value =>
        (if (value.forall(_.isValid))
           ToastCtx[F].clear() >>
             odbApi
               .updateObservations(
                 obsIds.toList,
                 ObservationPropertiesInput(
                   timingWindows = value.filter(_.isValid).map(_.toInput).assign
                 )
               )
               .toastErrors
               .void
         else
           ToastCtx[F].clear() *> ToastCtx[F]
             .showToast(
               "Invalid timing window(s), changes not saved",
               severity = Message.Severity.Error,
               sticky = true
             )
             .void).runAsync
