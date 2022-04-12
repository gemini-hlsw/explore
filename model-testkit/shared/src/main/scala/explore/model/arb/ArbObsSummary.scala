// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import lucuma.core.util.arb.ArbEnumerated._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import explore.model.ConstraintsSummary
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.util.arb.ArbGid._
import lucuma.core.enum.ObsStatus
import java.time.Duration
import explore.model.ObsSummaryWithConstraints
import explore.model.ObsSummaryWithTitleAndConstraints
import lucuma.core.arb.ArbTime
import lucuma.core.enum.ObsActiveStatus
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.scalacheck.string._

trait ArbObsSummary {
  import ArbConstraintsSummary._
  import ArbTime._

  implicit val arbObsSummaryWithConstraints = Arbitrary[ObsSummaryWithConstraints] {
    for {
      id           <- arbitrary[Observation.Id]
      constraints  <- arbitrary[ConstraintsSummary]
      status       <- arbitrary[ObsStatus]
      activeStatus <- arbitrary[ObsActiveStatus]
      duration     <- arbitrary[Duration]
      targets      <- arbitrary[Set[Target.Id]]
    } yield ObsSummaryWithConstraints(id, constraints, status, activeStatus, duration, targets)
  }

  implicit val arbObsSummaryWithTargetsAndConstraints =
    Arbitrary[ObsSummaryWithTitleAndConstraints] {
      for {
        id           <- arbitrary[Observation.Id]
        title        <- arbitrary[NonEmptyString]
        subtitle     <- arbitrary[Option[NonEmptyString]]
        constraints  <- arbitrary[ConstraintsSummary]
        status       <- arbitrary[ObsStatus]
        activeStatus <- arbitrary[ObsActiveStatus]
        duration     <- arbitrary[Duration]
      } yield ObsSummaryWithTitleAndConstraints(
        id,
        title,
        subtitle,
        constraints,
        status,
        activeStatus,
        duration
      )
    }

  implicit val cogenObsSummaryWithConstraints: Cogen[ObsSummaryWithConstraints] =
    Cogen[(Observation.Id, ConstraintsSummary, ObsStatus, Duration)]
      .contramap(o => (o.id, o.constraints, o.status, o.duration))

  implicit val cogenObsSummaryWithTargetsAndConstraints: Cogen[ObsSummaryWithTitleAndConstraints] =
    Cogen[
      (Observation.Id, String, Option[String], ConstraintsSummary, ObsStatus, Duration)
    ]
      .contramap(o =>
        (o.id, o.title.value, o.subtitle.map(_.value), o.constraints, o.status, o.duration)
      )
}

object ArbObsSummary extends ArbObsSummary
