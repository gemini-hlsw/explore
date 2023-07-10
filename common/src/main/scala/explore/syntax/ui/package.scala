// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.syntax.ui

import _root_.react.primereact.Message
import cats.*
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import clue.ResponseException
import clue.js.FetchJSRequest
import crystal.*
import crystal.react.*
import explore.model.Constants
import explore.model.UserVault
import explore.utils.*
import japgolly.scalajs.react.callback.Callback
import japgolly.scalajs.react.util.Effect
import org.scalajs.dom.Window
import org.typelevel.log4cats.Logger

import scala.scalajs.js
import scala.scalajs.js.UndefOr

extension (self: Window)
  // Keep this as def to take the value window.innerWidth at the current time
  def canFitTwoPanels: Boolean =
    self.innerWidth > Constants.TwoPanelCutoff

extension [A](c: js.UndefOr[A => Callback])
  def toJs: js.UndefOr[js.Function1[A, Unit]] = c.map(x => (a: A) => x(a).runNow())

extension [F[_]: MonadThrow](c: Logger[F])
  def pdebug[T](a: T): F[Unit] = c.debug(_root_.pprint.apply(a).render)

  def pdebugCB[T](a: T)(using Effect.Dispatch[F]): Callback =
    c.debug(_root_.pprint.apply(a).render).runAsyncAndForget

  def pinfo[T](a: T): F[Unit] = c.info(_root_.pprint.apply(a).render)

  def pinfoCB[T](a: T)(using Effect.Dispatch[F]): Callback =
    c.info(_root_.pprint.apply(a).render).runAsyncAndForget

extension (vault: UserVault)
  def authorizationHeader: String = s"Bearer ${vault.token.value}"

  def addAuthorizationHeader(request: FetchJSRequest): FetchJSRequest =
    // DOM Headers are mutable
    request.headers.set("Authorization", authorizationHeader)
    request

extension [F[_]: ApplicativeThrow: ToastCtx, A](f: F[A])
  def toastErrors: F[A] =
    f.onError {
      case ResponseException(errors, _) =>
        errors
          .map(e =>
            ToastCtx[F]
              .showToast(e.message, Message.Severity.Error, sticky = true)
          )
          .sequence
          .void
      case throwable                    =>
        ToastCtx[F]
          .showToast(throwable.getMessage, Message.Severity.Error, sticky = true)
    }

extension [F[_]: MonadCancelThrow, A](f: F[A])

  /**
   * Switch the value of a ViewF to true while executing the given effect, then switch it back to
   * false when the effect is finished
   */
  def switching(
    view: ViewF[F, Boolean]
  ): F[A] = switching(view, true, false)

  /**
   * Switch the value of a ViewF to true-ish (by the function) while executing the given effect,
   * then switch it back to false when the effect is finished
   */
  def switching[B](view: ViewF[F, B], boolToB: Boolean => B): F[A] =
    switching(view, boolToB(true), boolToB(false))

  /**
   * Switch the value of a ViewF to the @param acquire value while executing the given effect, then
   * switch it back to @param release when the effect is finished
   */
  def switching[B](view: ViewF[F, B], acquire: B, release: B): F[A] =
    MonadCancelThrow[F].bracket(view.set(acquire))(_ => f)(_ => view.set(release))
