// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import cats.effect.IO

import scala.scalajs.js
import org.scalajs.dom

import js.annotation._

import cats.effect.unsafe.implicits._

@JSExportTopLevel("IDBWorker", moduleID = "worker")
object IDBWorker {

  @JSExport
  def runWorker(): Unit = run.unsafeRunAndForget()

  def run: IO[Unit] = IO {
    val self = dom.DedicatedWorkerGlobalScope.self
    self.onmessage = (msg: dom.MessageEvent) => {
      println(s"Message arrived: ${msg.data}")
      self.postMessage(90)
    }
  }
}
