// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.syntax.ui

import cats.*
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import clue.ResponseException
import clue.js.FetchJsRequest
import crystal.*
import crystal.react.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.components.ui.PartnerFlags
import explore.model.Constants
import explore.optics.GetAdjust
import explore.utils.*
import japgolly.scalajs.react.callback.Callback
import japgolly.scalajs.react.util.Effect
import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Partner
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.Access
import lucuma.core.model.GuestRole
import lucuma.core.model.User
import lucuma.core.util.Enumerated
import lucuma.react.primereact.Message
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.*
import org.http4s.headers.Authorization
import org.scalajs.dom.Window
import org.typelevel.log4cats.Logger

import scala.scalajs.js

extension (self: Window)
  // Keep this as def to take the value window.innerWidth at the current time
  def canFitTwoPanels: Boolean =
    self.innerWidth > Constants.TwoPanelCutoff

extension [A](c: js.UndefOr[A => Callback])
  def toJs: js.UndefOr[js.Function1[A, Unit]] = c.map(x => (a: A) => x(a).runNow())

extension [F[_]](c: Logger[F])
  def pdebug[T](a: T): F[Unit] = c.debug(_root_.pprint.apply(a).render)

  def pdebugCB[T](a: T)(using Effect.Dispatch[F], MonadThrow[F]): Callback =
    c.debug(_root_.pprint.apply(a).render).runAsyncAndForget

  def pinfo[T](a: T): F[Unit] = c.info(_root_.pprint.apply(a).render)

  def pinfoCB[T](a: T)(using Effect.Dispatch[F], MonadThrow[F]): Callback =
    c.info(_root_.pprint.apply(a).render).runAsyncAndForget

extension (vault: UserVault)
  def addAuthorizationHeaderTo(request: FetchJsRequest): FetchJsRequest =
    // DOM Headers are mutable
    request.headers.set(
      Authorization.name.toString,
      vault.authorizationHeader.credentials.renderString
    )
    request

  def isStaff: Boolean = vault.user.role.access === Access.Staff

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

extension [A](pot: Pot[A])
  def orSpinner(f: A => VdomNode): VdomNode =
    pot.renderPot(valueRender = f, pendingRender = Icons.Spinner.withSpin(true))

extension (partner: Partner)
  def renderFlag: VdomNode =
    <.img(
      ^.src := PartnerFlags.smallFlag(partner),
      ^.alt := s"${partner.shortName} Flag",
      ExploreStyles.PartnerSplitFlag
    )

extension (tac: TimeAccountingCategory)
  def partner: Option[Partner]     = Enumerated[Partner].all.find(_.timeAccountingCategory === tac)
  def renderFlag: Option[VdomNode] = partner.map(_.renderFlag)

extension (vault: Option[UserVault])
  def userId: Option[User.Id] = vault.map(_.user).map(_.id)
  def isGuest: Boolean        = vault.map(_.user).map(_.role === GuestRole).getOrElse(true)
  def isStaff: Boolean        = vault.exists(_.isStaff)

extension [F[_], A](view: ViewF[F, A])
  def zoom[B](getAdjust: GetAdjust[A, B]): ViewF[F, B] =
    view.zoom(getAdjust.get)(getAdjust.mod)
