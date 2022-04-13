// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.annotation.nowarn
import japgolly.scalajs.react.callback.Callback
import japgolly.scalajs.react.vdom.html_<^._
import react.toastify._

@JSExportTopLevel("ExplorePWA")
object ExplorePWA {
  type UpdateSW = Boolean => js.Any

  @js.native
  trait ServiceWorkerRegistration extends js.Object {
    def update(): Unit = js.native
  }

  @js.native
  trait RegisterSWOptions extends js.Object {
    var onRegisterError: js.Function1[js.Any, Unit]
    var onRegistered: js.Function1[ServiceWorkerRegistration, Unit]
    var onNeedRefresh: js.Function0[Unit]
    var onOfflineReady: js.Function0[Unit]
  }

  object RegisterSWOptions {
    def apply(
      onNeedRefresh:   js.UndefOr[Callback] = js.undefined,
      onOfflineReady:  js.UndefOr[Callback] = js.undefined,
      onRegistered:    js.UndefOr[ServiceWorkerRegistration => Callback] = js.undefined,
      onRegisterError: js.UndefOr[js.Any => Callback] = js.undefined
    ): RegisterSWOptions = {
      val p = (new js.Object).asInstanceOf[RegisterSWOptions]
      onNeedRefresh.foreach(q => p.onNeedRefresh = () => { println("CAll"); q.runNow() })
      onOfflineReady.foreach(q => p.onOfflineReady = () => q.runNow())
      onRegisterError.foreach(q => p.onRegisterError = (x: js.Any) => q(x).runNow())
      onRegistered.foreach(q => p.onRegistered = (x: ServiceWorkerRegistration) => q(x).runNow())
      p
    }
  }

  @js.native
  @JSImport("virtual:pwa-register", "registerSW")
  object registerSW extends js.Object {
    @nowarn
    def apply(
      options: js.UndefOr[RegisterSWOptions] = js.undefined
    ): js.Function1[js.UndefOr[Boolean], js.Promise[Unit]] = js.native
  }
  val intervalMS = 2 * 60 * 1000

  def scheduleUpdate(r: ServiceWorkerRegistration): Callback =
    (Callback.log("abc") *> Callback(r.update()))
      .delayMs(intervalMS.toDouble)
      .toCallback
      .flatMap(_ => scheduleUpdate(r))

  def setupSW(): Callback =
    Callback.log("Setup service worker 22") *> Callback {
      lazy val updateSW: js.Function1[js.UndefOr[Boolean], js.Promise[Unit]] =
        registerSW(
          RegisterSWOptions(
            // onNeedRefresh = Callback(org.scalajs.dom.window.console.log(updateSW(true))),
            onNeedRefresh = Callback.log("Service worker") /* *> Callback(
              toast.info(<.div("Update available, reload"),
                         ToastOptions("exploreUpdateId",
                                      false,
                                      onClose = Callback.log("Closeed") *> Callback(updateSW(true))
                         )
              )*/
            ,
            onOfflineReady = Callback.log(s"Offline ready"),
            onRegisterError = (x: js.Any) =>
              Callback.log(s"Error on sw $x") *> Callback(
                org.scalajs.dom.window.console.log(x)
              ),
            onRegistered = (r: ServiceWorkerRegistration) =>
              Callback.log(s"Registered sw") *>
                Callback(r.update()) *>
                Callback(
                  org.scalajs.dom.window.console.log(r)
                ) *> (Callback.log("abc") *> Callback(r.update()))
                  .delayMs(intervalMS.toDouble)
                  .toCallback
          )
        )
      updateSW
    }

  @JSExport
  def runServiceWorker(): Unit =
    setupSW().handleError(t => Callback(t.printStackTrace())).runNow()

}
