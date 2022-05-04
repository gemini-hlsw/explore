// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import monocle.Focus
import monocle.Lens
import org.typelevel.cats.time._

import java.time.LocalDateTime

/**
 * This class is used to transfer properties across tiles, to reduce the latency while waiting for
 * the db subscriptions to arrive
 */
final case class ObsConfiguration(
  posAngle: PosAngle,
  obsTime:  LocalDateTime
)

object ObsConfiguration {
  implicit val eqObsConfiguration: Eq[ObsConfiguration] =
    Eq.by(x => (x.posAngle, x.obsTime))

  val posAngle: Lens[ObsConfiguration, PosAngle] =
    Focus[ObsConfiguration](_.posAngle)

  val obsTime: Lens[ObsConfiguration, LocalDateTime] =
    Focus[ObsConfiguration](_.obsTime)
}
