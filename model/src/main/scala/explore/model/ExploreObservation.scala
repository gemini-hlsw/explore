// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import java.time.Duration
import java.util.UUID

import cats.Eq
import cats.implicits._
import explore.model.enum.ObsStatus
import monocle.macros.Lenses

@Lenses
final case class ExploreObservation(
  id:          ExploreObservation.Id,
  target:      SiderealTarget,
  status:      ObsStatus,
  conf:        String,
  constraints: String,
  duration:    Duration
)

object ExploreObservation {
  type Id = UUID

  implicit val equalObservation: Eq[ExploreObservation] =
    Eq.by(_.id)
}
