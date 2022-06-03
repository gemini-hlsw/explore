// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import lucuma.core.model.PosAngle
import monocle.Focus
import monocle.Lens
import org.typelevel.cats.time._

import java.time.Instant

/**
 * This class is used to transfer properties across tiles, to reduce the latency while waiting for
 * the db subscriptions to arrive
 */
final case class ObsConfiguration(
  posAngle:   PosAngle,
  obsInstant: Option[Instant]
)

object ObsConfiguration {
  implicit val eqObsConfiguration: Eq[ObsConfiguration] =
    Eq.by(x => (x.posAngle, x.obsInstant))

  val posAngle: Lens[ObsConfiguration, PosAngle] =
    Focus[ObsConfiguration](_.posAngle)

  val obsInstant: Lens[ObsConfiguration, Option[Instant]] =
    Focus[ObsConfiguration](_.obsInstant)
}
