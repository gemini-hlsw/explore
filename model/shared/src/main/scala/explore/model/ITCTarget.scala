// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Hash
import lucuma.core.enum.MagnitudeBand
import lucuma.core.math.RadialVelocity
import lucuma.core.model.Magnitude

import scala.collection.immutable.SortedMap

final case class ITCTarget(rv: RadialVelocity, magnitudes: SortedMap[MagnitudeBand, Magnitude])

object ITCTarget {
  // We may consider adjusting this to consider small variations of RV identical for the
  // purpose of doing ITC calculations
  implicit val hashRadialVelocity: Hash[RadialVelocity] = Hash.by(_.rv.value)
  implicit val hashBand: Hash[MagnitudeBand]            = Hash.by(_.tag)
  implicit val hashMag: Hash[Magnitude]                 = Hash.fromUniversalHashCode
  implicit val hashITCTarget: Hash[ITCTarget]           = Hash.by(x => (x.rv, x.magnitudes))
}
