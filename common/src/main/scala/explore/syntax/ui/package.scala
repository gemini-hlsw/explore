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
import explore.model.syntax.all.*
import explore.optics.GetAdjust
import explore.utils.*
import japgolly.scalajs.react.callback.Callback
import japgolly.scalajs.react.util.Effect
import japgolly.scalajs.react.vdom.TagOf
import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Partner
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.GuestRole
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.CalculatedValue
import lucuma.core.util.Enumerated
import lucuma.react.fa.FontAwesomeIcon
import lucuma.react.primereact.Message
import lucuma.react.primereact.Tooltip
import lucuma.react.primereact.tooltip.*
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.pot.*
import org.http4s.headers.Authorization
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.Window
import org.typelevel.log4cats.Logger

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

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

// TODO: Move to lucuma-react?
extension (tag:    TagOf[HTMLElement])
  def withTooltipWhen(
    condition: Boolean,
    content:   VdomNode,
    position:  Option[Tooltip.Position] = None
  ): VdomNode =
    if (condition) tag.withTooltip(content = content, position = position.orUndefined) else tag
  def withTooltipUnless(
    condition: Boolean,
    content:   VdomNode,
    position:  Option[Tooltip.Position] = None
  ): VdomNode =
    withTooltipWhen(!condition, content, position)
  def withOptionalTooltip(
    content:  Option[VdomNode],
    position: Option[Tooltip.Position] = None
  ): VdomNode =
    content.fold(tag)(t => tag.withTooltip(content = t, position = position.orUndefined))

extension [A](calc: CalculatedValue[A])
  def staleClass: TagMod                            = ExploreStyles.Stale.when(calc.isStale)
  // Buttons require string tooltips :(
  def staleTooltipString: Option[String]            =
    if (calc.isStale) "Awaiting new data from server.".some else none
  def staleTooltip: Option[VdomNode]                = staleTooltip("Awaiting new data from server.")
  def staleTooltip(tip: VdomNode): Option[VdomNode] =
    if (calc.isStale) tip.some else none
  def renderOrElse(
    ready: A => VdomNode,
    stale: => VdomNode
  ): VdomNode =
    if (calc.isStale) stale else ready(calc.value)

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
  def isStaffOrAdmin: Boolean = vault.exists(_.isStaffOrAdmin)

extension [F[_], A](view: ViewF[F, A])
  def zoom[B](getAdjust: GetAdjust[A, B]): ViewF[F, B] =
    view.zoom(getAdjust.get)(getAdjust.mod)

extension [A](view: View[Option[A]])
  // If the view contains `none`, `get` returns the defaultDisplay value. When setting,
  // if the new value is the defaultSet value, set it to none.
  def withDefault(defaultSet: A, defaultDisplay: A)(using Eq[A]): View[Option[A]] =
    view.zoom(_.orElse(defaultDisplay.some))(f =>
      b => f(b).flatMap(newB => if (newB === defaultSet) none else newB.some)
    )

  def withDefault(default: A)(using Eq[A]): View[Option[A]] =
    withDefault(default, default)

  def removeOptionality(default: A): View[A] =
    view.zoom(_.getOrElse(default))(f => b => f(b.getOrElse(default)).some)

  // Treats None as the monoidic empty and vice versa.
  def withNoneAsEmpty(using m: Monoid[A], eq: Eq[A]): View[A] =
    // not lawful (can't get back a Some(empty)), but works for our purpose.
    val iso = monocle.Iso[Option[A], A](_.orEmpty)(_.some.filterNot(_.isEmpty))
    view.as(iso)

extension (target: Target)
  def icon: FontAwesomeIcon =
    target match
      case Target.Sidereal(_, _, _, _) => Icons.Star
      case Target.Nonsidereal(_, _, _) => Icons.PlanetRinged
      case Target.Opportunity(_, _, _) => Icons.HourglassClock
