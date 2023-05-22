// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

/**
 * The ResizeObserver constructor creates a new ResizeObserver object, which can be used to report
 * changes to the content or border box of an Element or the bounding box of an SVGElement
 *
 * @param callback
 *   The function called whenever an observed resize occurs.
 */
@js.native
@JSGlobal
class IntersectionObserver(
  callback: js.Function2[js.Array[IntersectionObserverEntry], IntersectionObserver, Unit],
  options:  js.UndefOr[IntersectionObserverOptions] = js.undefined
) extends js.Object {

  /** Starts observing the specified Element or SVGElement. */
  def observe(target: Element): Unit = js.native

  /** Unobserves all observed Element or SVGElement targets. */
  def disconnect(): Unit = js.native

  /** Ends the observing of a specified Element or SVGElement. */
  def unobserve(target: Element): Unit = js.native
}
