// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import monocle.Lens
import gsp.math.ProperMotion
import gsp.math.RightAscension
import gsp.math.Coordinates
import gsp.math.Declination

/**
  * Contains a set of useful optics to explore the model
  */
trait ModelOptics {

  /**
    * Lens for right ascension of a ProperMotion
    */
  val properMotionRA: Lens[ProperMotion, RightAscension] =
    ProperMotion.baseCoordinates ^|-> Coordinates.rightAscension

  /**
    * Lens for declination of a ProperMotion
    */
  val properMotionDec: Lens[ProperMotion, Declination] =
    ProperMotion.baseCoordinates ^|-> Coordinates.declination

  /**
    * Lens to an optional RightAscension of a target, it is unlawful as we can only
    * use it on sidereal targets
    */
  val targetRA: Lens[SiderealTarget, RightAscension] =
    SiderealTarget.track ^|-> properMotionRA

  val targetDec: Lens[SiderealTarget, Declination] =
    SiderealTarget.track ^|-> properMotionDec

  val targetPropsL =
    Lens[SiderealTarget, (String, RightAscension, Declination)](t =>
      (t.name, targetRA.get(t), targetDec.get(t))
    )(s => t => targetRA.set(s._2)(targetDec.set(s._3)(t.copy(name = s._1))))
}

object ModelOptics extends ModelOptics
