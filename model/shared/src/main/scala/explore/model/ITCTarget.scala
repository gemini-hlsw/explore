// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Hash
import lucuma.core.math.RadialVelocity

final case class ITCTarget(rv: RadialVelocity)

object ITCTarget {
  // We may consider adjusting this to consider small variations of RV identical for the
  // purpose of doing ITC calculations
  implicit val hashRadialVelocity: Hash[RadialVelocity] = Hash.by(_.rv.value)
  implicit val hashITCTarget: Hash[ITCTarget]           = Hash.by(x => x.rv)
}
