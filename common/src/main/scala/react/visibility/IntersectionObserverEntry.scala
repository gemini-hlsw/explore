// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package org.scalajs.dom

import scala.scalajs.js

/**
 * The IntersectionObserverEntry interface of the Intersection Observer API describes the
 * intersection between the target element and its root container at a specific moment of
 * transition.
 */
@js.native
trait IntersectionObserverEntry extends js.Object {

  /**
   * A Boolean value which is true if the target element intersects with the intersection observer's
   * root. If this is true, then, the IntersectionObserverEntry describes a transition into a state
   * of intersection; if it's false, then you know the transition is from intersecting to
   * not-intersecting.
   */
  def isIntersecting: Boolean = js.native

}
