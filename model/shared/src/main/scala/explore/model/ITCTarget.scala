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
