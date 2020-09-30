// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import eu.timepit.refined.types.string.NonEmptyString
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

  /**
   * Lens to the RightAscension of a sidereal target
   */
  val targetRA: Lens[SiderealTarget, RightAscension] =
    SiderealTarget.track ^|-> properMotionRA

  /**
   * Lens to the Declination of a sidereal target
   */
  val targetDec: Lens[SiderealTarget, Declination] =
    SiderealTarget.track ^|-> properMotionDec

  /**
   * Lens used to change name and coordinates of a target
   */
  val targetPropsL =
    Lens[SiderealTarget, (NonEmptyString, RightAscension, Declination)](t =>
      (t.name, targetRA.get(t), targetDec.get(t))
    )(s => t => targetRA.set(s._2)(targetDec.set(s._3)(t.copy(name = s._1))))
}

object ModelOptics extends ModelOptics
