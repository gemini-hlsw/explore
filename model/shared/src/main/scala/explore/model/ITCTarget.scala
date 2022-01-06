// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Hash
import lucuma.core.enum.Band
import lucuma.core.math.BrightnessValue
import lucuma.core.math.RadialVelocity
import lucuma.core.math.dimensional.Measure

import scala.collection.immutable.SortedMap

final case class ITCTarget(
  rv:           RadialVelocity,
  brightnesses: SortedMap[Band, Measure[BrightnessValue]]
)

object ITCTarget {
  // We may consider adjusting this to consider small variations of RV identical for the
  // purpose of doing ITC calculations
  implicit val hashRadialVelocity: Hash[RadialVelocity]              = Hash.by(_.rv.value)
  implicit val hashBand: Hash[Band]                                  = Hash.by(_.tag)
  implicit val hashBrightnessMeasure: Hash[Measure[BrightnessValue]] = Hash.fromUniversalHashCode
  implicit val hashITCTarget: Hash[ITCTarget]                        = Hash.by(x => (x.rv, x.brightnesses))
}
