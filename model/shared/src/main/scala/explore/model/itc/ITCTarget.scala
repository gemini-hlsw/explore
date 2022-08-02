// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.itc

import cats.Hash
import lucuma.core.enums.Band
import lucuma.core.math.RadialVelocity
import lucuma.core.model.SourceProfile

final case class ItcTarget(
  rv:      RadialVelocity,
  profile: SourceProfile
)

object ItcTarget {
  // We may consider adjusting this to consider small variations of RV identical for the
  // purpose of doing ITC calculations
  implicit val hashRadialVelocity: Hash[RadialVelocity] = Hash.by(_.rv.value)
  implicit val hashBand: Hash[Band]                     = Hash.by(_.tag)
  implicit val hashProfile: Hash[SourceProfile]         = Hash.fromUniversalHashCode
  implicit val hashITCTarget: Hash[ItcTarget]           = Hash.by(x => (x.rv, x.profile))
}
