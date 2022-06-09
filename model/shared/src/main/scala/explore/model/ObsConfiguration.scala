// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import lucuma.core.model.PosAngleConstraint
import monocle.Focus
import monocle.Lens

/**
 * This class is used to transfer properties across tiles, to reduce the latency while waiting for
 * the db subscriptions to arrive
 */
final case class ObsConfiguration(
  posAngl: PosAngleConstraint
)

object ObsConfiguration {
  implicit val eqObsConfiguration: Eq[ObsConfiguration] =
    Eq.by(_.posAngl)

  val posAngle: Lens[ObsConfiguration, PosAngleConstraint] =
    Focus[ObsConfiguration](_.posAngl)

}
