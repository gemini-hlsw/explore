// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import lucuma.core.util.arb.ArbEnumerated._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import explore.model.ConstraintsSummary
import lucuma.core.model.Observation
import lucuma.core.util.arb.ArbGid._
import lucuma.core.enum.ObsStatus
import java.time.Duration
import explore.model.ObsSummaryWithConstraints
import explore.model.ObsSummaryWithPointingAndConstraints
import explore.model.Pointing
import lucuma.core.arb.ArbTime

trait ArbObsSummary {
  import ArbConstraintsSummary._
  import ArbPointing._
  import ArbTime._

  implicit val arbObsSummaryWithConstraints = Arbitrary[ObsSummaryWithConstraints] {
    for {
      id          <- arbitrary[Observation.Id]
      constraints <- arbitrary[Option[ConstraintsSummary]]
      status      <- arbitrary[ObsStatus]
      duration    <- arbitrary[Duration]
    } yield ObsSummaryWithConstraints(id, constraints, status, duration)
  }

  implicit val arbObsSummaryWithPointingAndConstraints =
    Arbitrary[ObsSummaryWithPointingAndConstraints] {
      for {
        id          <- arbitrary[Observation.Id]
        pointing    <- arbitrary[Option[Pointing]]
        constraints <- arbitrary[Option[ConstraintsSummary]]
        status      <- arbitrary[ObsStatus]
        duration    <- arbitrary[Duration]
      } yield ObsSummaryWithPointingAndConstraints(id, pointing, constraints, status, duration)
    }

  implicit val cogenObsSummaryWithConstraints: Cogen[ObsSummaryWithConstraints] =
    Cogen[(Observation.Id, Option[ConstraintsSummary], ObsStatus, Duration)]
      .contramap(o => (o.id, o.constraints, o.status, o.duration))

  implicit val cogenObsSummaryWithPointingAndConstraints
    : Cogen[ObsSummaryWithPointingAndConstraints] =
    Cogen[(Observation.Id, Option[Pointing], Option[ConstraintsSummary], ObsStatus, Duration)]
      .contramap(o => (o.id, o.pointing, o.constraints, o.status, o.duration))
}

object ArbObsSummary extends ArbObsSummary
