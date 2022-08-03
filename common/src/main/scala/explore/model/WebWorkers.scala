// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

object WebWorkers {

  /**
   * This deserves an explanation:
   *
   * To make the webworker act correctly in both dev and production we shoud import it as a module
   * rather than just doing a direct consructor call.
   *
   * Doing the import with the "worker" param gives a constructor for the worker which we can wrap
   * inline lets us save some space keeping a single chunk More info see:
   * https://vitejs.dev/guide/features.html#import-with-query-suffixes=
   */
  @js.native
  @JSImport("/cacheworker.js?worker", JSImport.Default)
  object CacheIDBWorkerDevelopment extends js.Object {
    def apply(): dom.Worker = js.native
  }

  def CacheIDBWorkerProduction(): dom.Worker = new dom.Worker("/cacheworker.js")
}
