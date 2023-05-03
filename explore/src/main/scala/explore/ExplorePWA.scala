// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import explore.components.ui.ExploreStyles
import explore.events.ExploreEvent
import japgolly.scalajs.react.callback.AsyncCallback
import japgolly.scalajs.react.callback.Callback
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.broadcastchannel.*
import lucuma.typed.std.RequestInit
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import org.scalajs.dom
import org.scalajs.dom.Fetch
import org.scalajs.dom.Request
import react.fa.IconSize

import scala.annotation.nowarn
import scala.concurrent.ExecutionContext
import scala.scalajs.js
import scala.scalajs.js.annotation.*

@JSExportTopLevel("ExplorePWA")
object ExplorePWA {
  type UpdateSW = Boolean => js.Any

  // Facades for
  // https://vite-plugin-pwa.netlify.app/
  @js.native
  trait ServiceWorkerRegistration extends js.Object {
    def update(): Unit      = js.native
    def installing: Boolean = js.native
  }

  @js.native
  trait RegisterSWOptions extends js.Object {
    var onRegisterError: js.Function1[js.Any, Unit]
    var onRegisteredSW: js.Function2[String, ServiceWorkerRegistration, Unit]
    var onNeedRefresh: js.Function0[Unit]
    var onOfflineReady: js.Function0[Unit]
  }

  object RegisterSWOptions {
    def apply(
      onNeedRefresh:   js.UndefOr[Callback] = js.undefined,
      onOfflineReady:  js.UndefOr[Callback] = js.undefined,
      onRegisteredSW:  js.UndefOr[(String, ServiceWorkerRegistration) => Callback] = js.undefined,
      onRegisterError: js.UndefOr[js.Any => Callback] = js.undefined
    ): RegisterSWOptions = {
      val p = (new js.Object).asInstanceOf[RegisterSWOptions]
      onNeedRefresh.foreach(q =>
        p.onNeedRefresh =
          () => q.handleError(t => Callback.log(s"SW Error Refresh ${t.getMessage}")).runNow()
      )
      onOfflineReady.foreach(q =>
        p.onOfflineReady =
          () => q.handleError(t => Callback.log(s"SW Error Offline ready ${t.getMessage}")).runNow()
      )
      onRegisterError.foreach(q =>
        p.onRegisterError = (x: js.Any) =>
          q(x).handleError(t => Callback.log(s"SW Error register error ${t.getMessage}")).runNow()
      )
      onRegisteredSW.foreach(q =>
        p.onRegisteredSW = (u: String, x: ServiceWorkerRegistration) =>
          q(u, x).handleError(t => Callback.log(s"SW Error on Register ${t.getMessage}")).runNow()
      )
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

  // Check every 10 min maybe too often but we are doing lots of development
  val intervalMS = 10 * 60 * 1000

  def scheduleUpdateCheck(u: String, r: ServiceWorkerRegistration): Callback = {
    import scala.concurrent.ExecutionContext.Implicits.global
    (Callback.log("Check for new explore version") *>
      pingSW(u)
        .flatMap(go => Callback(r.update()).when(go).asAsyncCallback)
        .toCallback
        .attempt
        .void)
      .setIntervalMs(intervalMS.toDouble)
      .void
  }

  // Ask the user if an updates should be reloaded
  def requestUserConfirmation(bc: BroadcastChannel[ExploreEvent]): Callback = Callback {
    bc.postMessage(ExploreEvent.PWAUpdate)
  }.void

  def pingSW(url: String)(using ExecutionContext): AsyncCallback[Boolean] =
    AsyncCallback.fromFuture {
      import js.Thenable.Implicits.*
      val responseStatus =
        dom.fetch(Request(url)).map(_.status)
      responseStatus.map(_ == 200).recover { case _ => false }
    }

  // Setup the service worker
  def setupSW(): Callback =
    // Register the service worker and handle some callbacks
    Callback {
      val bc = new BroadcastChannel[ExploreEvent]("explore")

      bc.onmessage = (
        (x: ExploreEvent) =>
          // This is coming from the client
          x.event match {
            case ExploreEvent.PWAReloadId      =>
              IO(updateSW(true))
            case ExploreEvent.ExploreUIReadyId =>
              IO(updateSW)
            case a                             => IO.unit
          }
      ): (ExploreEvent => IO[Unit])

      lazy val updateSW: js.Function1[js.UndefOr[Boolean], js.Promise[Unit]] =
        registerSW(
          RegisterSWOptions(
            onNeedRefresh =
              // If a new version is detected ask the usser
              Callback.log("New version available, ask the user to update") *>
                // Delay a bit to let the front setup the listener
                requestUserConfirmation(bc),
            onOfflineReady = Callback.log(s"Offline ready"),
            onRegisterError = (x: js.Any) =>
              Callback.log(s"Error on service worker registration $x") *> Callback(
                org.scalajs.dom.window.console.log(x)
              ),
            onRegisteredSW = (u: String, r: ServiceWorkerRegistration) =>
              import scala.concurrent.ExecutionContext.Implicits.global
              // https://vite-plugin-pwa.netlify.app/guide/periodic-sw-updates.html
              val isOffline =
                !(!(r.installing && !js.isUndefined(org.scalajs.dom.window.navigator))) ||
                  (dom.window.navigator.hasOwnProperty("connection") && dom.window.navigator.onLine)
              Callback.log(s"Service worker registered, setup self update task") *>
                pingSW(u)
                  .flatMap(go =>
                    Callback(r.update())
                      .delayMs(2000.0)
                      .when(go)
                      .attempt
                  )
                  .toCallback *>          // Inital check
                scheduleUpdateCheck(u, r) // Periodic checks
          )
        )
    }

  @JSExport
  def runServiceWorker(): Unit =
    setupSW().handleError(t => Callback(t.printStackTrace())).runNow()

}
