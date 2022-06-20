// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import org.typelevel.cats.time.instantInstances
import eu.timepit.refined.cats._
import monocle.Focus

import java.time.Instant
import lucuma.core.model.PosAngleConstraint

case class ObsConfiguration(
  vizTime:            Instant,
  posAngleConstraint: Option[PosAngleConstraint],
  scienceMode:        Option[ScienceMode]
) {
  def hasPosAngleConstraint: Boolean = posAngleConstraint.isDefined
}

object ObsConfiguration {
  val vizTime              = Focus[ObsConfiguration](_.vizTime)
  val userSelectionMessage = Focus[ObsConfiguration](_.posAngleConstraint)
  val searchingTarget      = Focus[ObsConfiguration](_.scienceMode)

  implicit val eqRootModel: Eq[ObsConfiguration] =
    Eq.by(m => (m.vizTime, m.posAngleConstraint, m.scienceMode))
}
