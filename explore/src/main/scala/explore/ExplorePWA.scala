// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.callback.Callback
import japgolly.scalajs.react.vdom.html_<^._
import react.fa.IconSize
import react.semanticui.elements.button.Button
import react.semanticui.sizes
import react.toastify._

import scala.annotation.nowarn
import scala.scalajs.js
import scala.scalajs.js.annotation._

@JSExportTopLevel("ExplorePWA")
object ExplorePWA {
  type UpdateSW = Boolean => js.Any

  // Facades for
  // https://vite-plugin-pwa.netlify.app/
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
      onNeedRefresh.foreach(q => p.onNeedRefresh = () => q.runNow())
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

  // Check every 10 min maybe too often but we are doing lots of development
  val intervalMS = 10 * 60 * 1000

  def scheduleUpdateCheck(r: ServiceWorkerRegistration): Callback =
    (Callback.log("Update check") *> Callback(r.update())).attempt
      .setIntervalMs(intervalMS.toDouble)
      .void

  // Toast to ask the user if an updates should be reloadedd
  def showToast(reload: Callback) =
    toast(
      <.div(
        ExploreStyles.ExploreToastGrid,
        Icons.Info.size(IconSize.LG),
        "A new version of explore is available",
        Button(size = sizes.Mini, onClick = reload *> toast.dismissCB("exploreUpdateId"))("Reload")
      ),
      ToastOptions("exploreUpdateId", autoClose = false)
    )

  // Setup the service worker
  def setupSW(): Callback =
    // Register the service worker and handle some callbacks
    Callback {
      lazy val updateSW: js.Function1[js.UndefOr[Boolean], js.Promise[Unit]] =
        registerSW(
          RegisterSWOptions(
            onNeedRefresh =
              // If a new version is detected ask the usser
              Callback.log("New version avaiable") *> Callback(showToast(Callback(updateSW(true)))),
            onOfflineReady = Callback.log(s"Offline ready"),
            onRegisterError = (x: js.Any) =>
              Callback.log(s"Error on sw $x") *> Callback(
                org.scalajs.dom.window.console.log(x)
              ),
            onRegistered = (r: ServiceWorkerRegistration) =>
              // https://vite-plugin-pwa.netlify.app/guide/periodic-sw-updates.html
              Callback.log(s"Registered service worker") *>
                Callback(r.update()).delayMs(1000.0).toCallback *> // Inital check
                scheduleUpdateCheck(r) // Periodic checks
          )
        )
      updateSW
    }

  @JSExport
  def runServiceWorker(): Unit =
    setupSW().handleError(t => Callback(t.printStackTrace())).runNow()

}
