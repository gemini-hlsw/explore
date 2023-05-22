// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.scalajs.dom

import scala.scalajs.js
import org.scalajs.dom

trait IntersectionObserverOptions extends js.Object {
  var root: js.UndefOr[dom.Element]                    = js.undefined
  var rootMargin: js.UndefOr[String]                   = js.undefined
  var threshold: js.UndefOr[Double | js.Array[Double]] = js.undefined
}

object IntersectionObserverOptions {
  def apply(
    root:       js.UndefOr[dom.Element] = js.undefined,
    rootMargin: js.UndefOr[String] = js.undefined,
    threshold:  js.UndefOr[Double | js.Array[Double]] = js.undefined
  ): IntersectionObserverOptions =
    val p = (new js.Object()).asInstanceOf[IntersectionObserverOptions]
    p.root = root
    p

}
