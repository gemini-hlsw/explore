// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.scalacheck.string._
import explore.model.ConstraintsSummary
import explore.model.ObsSummaryWithConstraints
import explore.model.ObsSummaryWithTitleAndConstraints
import explore.model.ObsSummaryWithConstraintsAndConf
import explore.model.ObsSummaryWithTitleConstraintsAndConf
import explore.model.ScienceConfiguration
import lucuma.core.enum.ObsActiveStatus
import lucuma.core.enum.ObsStatus
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.arb.ArbTime
import lucuma.core.util.arb.ArbGid._
import lucuma.core.util.arb.ArbEnumerated._
import java.time.Duration

trait ArbObsSummary {
  import ArbConstraintsSummary._
  import ArbTime._
  import ArbScienceConfiguration._

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

  implicit val arbObsSummaryWithConstraintsAndConf =
    Arbitrary[ObsSummaryWithConstraintsAndConf] {
      for {
        id            <- arbitrary[Observation.Id]
        constraints   <- arbitrary[ConstraintsSummary]
        status        <- arbitrary[ObsStatus]
        activeStatus  <- arbitrary[ObsActiveStatus]
        duration      <- arbitrary[Duration]
        targets       <- arbitrary[Set[Target.Id]]
        configuration <- arbitrary[Option[ScienceConfiguration]]
      } yield ObsSummaryWithConstraintsAndConf(
        id,
        constraints,
        status,
        activeStatus,
        duration,
        targets,
        configuration
      )
    }

  implicit val arbObsSummaryWithTitleConstraintsAndConf =
    Arbitrary[ObsSummaryWithTitleConstraintsAndConf] {
      for {
        id            <- arbitrary[Observation.Id]
        title         <- arbitrary[NonEmptyString]
        subtitle      <- arbitrary[Option[NonEmptyString]]
        constraints   <- arbitrary[ConstraintsSummary]
        status        <- arbitrary[ObsStatus]
        activeStatus  <- arbitrary[ObsActiveStatus]
        duration      <- arbitrary[Duration]
        configuration <- arbitrary[Option[ScienceConfiguration]]
      } yield ObsSummaryWithTitleConstraintsAndConf(
        id,
        title,
        subtitle,
        constraints,
        status,
        activeStatus,
        duration,
        configuration
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

  implicit val cogenObsSummaryWithConstraintsAndConf: Cogen[ObsSummaryWithConstraintsAndConf] =
    Cogen[
      (Observation.Id,
       ConstraintsSummary,
       ObsStatus,
       ObsActiveStatus,
       Duration,
       List[Target.Id],
       Option[ScienceConfiguration]
      )
    ]
      .contramap(o =>
        (o.id,
         o.constraints,
         o.status,
         o.activeStatus,
         o.duration,
         o.scienceTargetIds.toList,
         o.scienceConfiguration
        )
      )

  implicit val cogenObsSummaryWithTitleConstraintsAndConf
    : Cogen[ObsSummaryWithTitleConstraintsAndConf] =
    Cogen[
      (Observation.Id,
       String,
       Option[String],
       ConstraintsSummary,
       ObsStatus,
       ObsActiveStatus,
       Duration,
       Option[ScienceConfiguration]
      )
    ]
      .contramap(o =>
        (o.id,
         o.title.value,
         o.subtitle.map(_.value),
         o.constraints,
         o.status,
         o.activeStatus,
         o.duration,
         o.scienceConfiguration
        )
      )
}

object ArbObsSummary extends ArbObsSummary
