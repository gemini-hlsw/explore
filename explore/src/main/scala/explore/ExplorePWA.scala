// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import crystal.react.*
import explore.events.ExploreEvent
import fs2.Stream
import fs2.dom.BroadcastChannel
import japgolly.scalajs.react.callback.Callback
import lucuma.ui.syntax.all.*
import org.scalajs.dom
import org.scalajs.dom.Request

import java.util.concurrent.TimeUnit
import scala.annotation.nowarn
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration
import scala.scalajs.js
import scala.scalajs.js.annotation.*

@JSExportTopLevel("ExplorePWA", moduleID = "explore")
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
  val updateInteval = FiniteDuration(10, TimeUnit.MINUTES)

  def scheduleUpdateCheck(u: String, r: ServiceWorkerRegistration): IO[Unit] =
    Stream
      .awakeEvery[IO](updateInteval)
      .evalTap { _ =>
        IO.println("Check for new explore version") >>
          pingSW(u).ifM(IO(r.update()), IO.unit)
      }
      .compile
      .drain

  // Ask the user if an update should be reloaded
  def requestUserConfirmation(bc: BroadcastChannel[IO, ExploreEvent]): Callback =
    bc.postMessage(ExploreEvent.PWAUpdate).runAsyncAndForget

  def pingSW(url: String)(using ExecutionContext): IO[Boolean] = {
    import js.Thenable.Implicits.*
    val responseStatus =
      dom.fetch(Request(url)).map(_.status)
    IO.fromFuture(IO(responseStatus)).map(_ == 200).recover { case _ => false }
  }

  def handleEvents(bc: BroadcastChannel[IO, ExploreEvent]): IO[Unit] = {
    lazy val updateSW: js.Function1[js.UndefOr[Boolean], js.Promise[Unit]] =
      registerSW(
        RegisterSWOptions(
          onNeedRefresh =
            // If a new version is detected ask the usser
            Callback.log("New version available, ask the user to update") *>
              // Delay a bit to let the front setup the listener
              requestUserConfirmation(bc),
          onOfflineReady = Callback.log(s"Offline ready"),
          onRegisterError = (x: js.Any) => Callback.log(s"Error on service worker registration", x),
          onRegisteredSW = (u: String, r: ServiceWorkerRegistration) =>
            import scala.concurrent.ExecutionContext.Implicits.global
            // https://vite-plugin-pwa.netlify.app/guide/periodic-sw-updates.html
            val isOffline =
              ! !(r.installing && !js.isUndefined(org.scalajs.dom.window.navigator)) ||
                (dom.window.navigator.hasOwnProperty("connection") && dom.window.navigator.onLine)
            (IO.println(
              s"Service worker registered, check for updates every $updateInteval, offline: $isOffline"
            ) *>
              // initial check
              pingSW(u)
                .ifM(IO.sleep(Duration(2, TimeUnit.SECONDS)) >> IO(r.update()), IO.unit)
                .voidError *>
              scheduleUpdateCheck(u, r)).runAsyncAndForget
        )
      )

    bc.messages
      .evalTap { me =>
        me.data.event match
          case ExploreEvent.PWAReloadId      => IO.fromPromise(IO(updateSW(true))) // .void
          case ExploreEvent.ExploreUIReadyId => IO.fromPromise(IO(updateSW(())))   // .void
          case _                             => IO.unit
      }
      .compile
      .drain
  }

  // Setup the service worker
  def setupSW: IO[Unit] =
    (for {
      bc <- BroadcastChannel[IO, ExploreEvent]("explore")
      _  <- handleEvents(bc).background
    } yield ()).useForever

  @JSExport
  def runServiceWorker(): Unit =
    setupSW.unsafeRunAndForget()

}
