// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.SiderealTracking
import monocle.Lens

/**
 * Contains a set of useful optics to explore the model
 */
trait ModelOptics {

  /**
   * Lens for right ascension of a SiderealTracking
   */
  val properMotionRA: Lens[SiderealTracking, RightAscension] =
    SiderealTracking.baseCoordinates ^|-> Coordinates.rightAscension

  /**
   * Lens for declination of a SiderealTracking
   */
  val properMotionDec: Lens[SiderealTracking, Declination] =
    SiderealTracking.baseCoordinates ^|-> Coordinates.declination
}

object ModelOptics extends ModelOptics
