// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.syntax.ui

import cats.*
import cats.syntax.all.*
import clue.js.FetchJSRequest
import crystal.react.implicits.*
import explore.components.ui.ExploreStyles
import explore.model.Constants
import explore.model.UserVault
import explore.utils.*
import japgolly.scalajs.react.callback.Callback
import japgolly.scalajs.react.util.Effect
import japgolly.scalajs.react.vdom.VdomNode
import org.scalablytyped.runtime.StringDictionary
import org.scalajs.dom.Window
import org.typelevel.log4cats.Logger
import react.common.Css

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
