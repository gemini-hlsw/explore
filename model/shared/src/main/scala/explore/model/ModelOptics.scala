// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import lucuma.core.enum.Band
import lucuma.core.math.BrightnessValue
import lucuma.core.math.RadialVelocity
import lucuma.core.math.dimensional.Measure
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import monocle.Getter
import monocle.Optional

import scala.collection.immutable.SortedMap

/**
 * Contains a set of useful optics to explore the model
 */
trait ModelOptics {

  // TODO TEST!!!
  /** Direct optic into a defined RadialVelocity in a SiderealTarget */
  val targetRV: Optional[Target, RadialVelocity] =
    Target.sidereal.andThen(Target.Sidereal.radialVelocity.some)

  /**
   * Getter for any kind of brightness measures of a `Target`, as long as it has a
   * `SpectralDefinition.BandNormalized`
   */
  val targetBrightnesses: Getter[Target, Option[SortedMap[Band, Measure[BrightnessValue]]]] =
    Getter { target =>
      val sourceProfile = Target.sourceProfile.get(target)
      SourceProfile.integratedBrightnesses
        .getOption(sourceProfile)
        .orElse(SourceProfile.surfaceBrightnesses.getOption(sourceProfile))
    }
}

object ModelOptics extends ModelOptics
