// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect._
import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.annotation.nowarn
import japgolly.scalajs.react.callback.Callback

object ExplorePWA {
  @js.native
  trait RegisterSWOptions extends js.Object {
    var onRegisterError: js.Function1[js.Any, Unit]
    var onRegistered: js.Function1[js.Any, Unit]
    var onNeedRefresh: js.Function0[Unit]
    var onOfflineReady: js.Function0[Unit]
  }

  object RegisterSWOptions {
    def apply(
      onNeedRefresh:   js.UndefOr[Callback] = js.undefined,
      onOfflineReady:  js.UndefOr[Callback] = js.undefined,
      onRegistered:    js.UndefOr[js.Any => Callback] = js.undefined,
      onRegisterError: js.UndefOr[js.Any => Callback] = js.undefined
    ): RegisterSWOptions = {
      val p = (new js.Object).asInstanceOf[RegisterSWOptions]
      onNeedRefresh.foreach(q => p.onNeedRefresh = () => q.runNow())
      onOfflineReady.foreach(q => p.onOfflineReady = () => q.runNow())
      onRegisterError.foreach(q => p.onRegisterError = (x: js.Any) => q(x).runNow())
      onRegistered.foreach(q => p.onRegistered = (x: js.Any) => q(x).runNow())
      p
    }
  }

  @js.native
  @JSImport("virtual:pwa-register", "registerSW")
  object registerSW extends js.Object {
    @nowarn
    def apply(
      options: js.UndefOr[RegisterSWOptions] = js.undefined
    ): js.Function1[Boolean, js.Promise[Unit]] = js.native
  }

  def setupSW: IO[Unit] =
    IO.println("Setup service worker 2") *> IO {
      lazy val updateSW: js.Function1[Boolean, js.Promise[Unit]] = registerSW(
        RegisterSWOptions(
          onNeedRefresh = Callback.log(s"Need refresh") *> Callback(updateSW(true)),
          onOfflineReady = Callback.log(s"Offline ready"),
          onRegisterError = (x: js.Any) =>
            Callback.log(s"Error on sw $x") *> Callback(
              org.scalajs.dom.window.console.log(x)
            ),
          onRegistered = (x: js.Any) =>
            Callback.log(s"Registered sw") *> Callback(org.scalajs.dom.window.console.log(x))
        )
      )
      ()
    }.void

}
