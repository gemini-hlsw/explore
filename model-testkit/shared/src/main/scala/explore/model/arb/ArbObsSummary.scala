// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.scalacheck.string.*
import explore.model.BasicConfiguration
import explore.model.ObsSummaryWithConstraints
import explore.model.ObsSummaryWithTitleAndConstraints
import explore.model.ObsSummaryWithConstraintsAndConf
import explore.model.ObsSummaryWithTitleConstraintsAndConf
import explore.model.ObsSummaryWithTitleAndConf
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsStatus
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.arb.ArbTime
import lucuma.core.model.arb.ArbPosAngleConstraint.*
import lucuma.core.math.arb.ArbWavelength.*
import lucuma.core.util.arb.ArbGid.*
import lucuma.core.util.arb.ArbEnumerated.*
import lucuma.schemas.model.ConstraintsSummary
import java.time.Duration
import java.time.Instant
import lucuma.core.model.PosAngleConstraint
import lucuma.core.math.Wavelength
import lucuma.schemas.model.arb.ArbConstraintsSummary

trait ArbObsSummary {
  import ArbBasicConfiguration.given
  import ArbConstraintsSummary.given
  import ArbTime.*

  implicit val arbObsSummaryWithConstraints: Arbitrary[ObsSummaryWithConstraints] =
    Arbitrary[ObsSummaryWithConstraints] {
      for {
        id           <- arbitrary[Observation.Id]
        constraints  <- arbitrary[ConstraintsSummary]
        status       <- arbitrary[ObsStatus]
        activeStatus <- arbitrary[ObsActiveStatus]
        duration     <- arbitrary[Duration]
        targets      <- arbitrary[Set[Target.Id]]
      } yield ObsSummaryWithConstraints(id, constraints, status, activeStatus, duration, targets)
    }

  implicit val cogenObsSummaryWithConstraints: Cogen[ObsSummaryWithConstraints] =
    Cogen[
      (Observation.Id, ConstraintsSummary, ObsStatus, ObsActiveStatus, Duration, List[Target.Id])
    ]
      .contramap(o =>
        (o.id, o.constraints, o.status, o.activeStatus, o.duration, o.scienceTargetIds.toList)
      )

  implicit val arbObsSummaryWithTitleAndConstraints: Arbitrary[ObsSummaryWithTitleAndConstraints] =
    Arbitrary[ObsSummaryWithTitleAndConstraints] {
      for {
        id           <- arbitrary[Observation.Id]
        title        <- arbitrary[String]
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

  implicit val cogenObsSummaryWithTitleAndConstraints: Cogen[ObsSummaryWithTitleAndConstraints] =
    Cogen[
      (Observation.Id,
       String,
       Option[String],
       ConstraintsSummary,
       ObsStatus,
       ObsActiveStatus,
       Duration
      )
    ]
      .contramap(o =>
        (o.id,
         o.title,
         o.subtitle.map(_.value),
         o.constraints,
         o.status,
         o.activeStatus,
         o.duration
        )
      )

  implicit val arbObsSummaryWithTitleConstraintsAndConf
    : Arbitrary[ObsSummaryWithTitleConstraintsAndConf] =
    Arbitrary[ObsSummaryWithTitleConstraintsAndConf] {
      for {
        id            <- arbitrary[Observation.Id]
        title         <- arbitrary[String]
        subtitle      <- arbitrary[Option[NonEmptyString]]
        constraints   <- arbitrary[ConstraintsSummary]
        status        <- arbitrary[ObsStatus]
        activeStatus  <- arbitrary[ObsActiveStatus]
        duration      <- arbitrary[Duration]
        configuration <- arbitrary[Option[BasicConfiguration]]
        vizTime       <- arbitrary[Option[Instant]]
      } yield ObsSummaryWithTitleConstraintsAndConf(
        id,
        title,
        subtitle,
        constraints,
        status,
        activeStatus,
        duration,
        configuration,
        vizTime
      )
    }

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
       Option[BasicConfiguration]
      )
    ]
      .contramap(o =>
        (o.id,
         o.title,
         o.subtitle.map(_.value),
         o.constraints,
         o.status,
         o.activeStatus,
         o.duration,
         o.configuration
        )
      )

  implicit val arbObsSummaryWithTitleAndConf: Arbitrary[ObsSummaryWithTitleAndConf] =
    Arbitrary[ObsSummaryWithTitleAndConf] {
      for {
        id            <- arbitrary[Observation.Id]
        title         <- arbitrary[String]
        subtitle      <- arbitrary[Option[NonEmptyString]]
        status        <- arbitrary[ObsStatus]
        activeStatus  <- arbitrary[ObsActiveStatus]
        duration      <- arbitrary[Duration]
        configuration <- arbitrary[Option[BasicConfiguration]]
      } yield ObsSummaryWithTitleAndConf(
        id,
        title,
        subtitle,
        status,
        activeStatus,
        duration,
        configuration
      )
    }

  implicit val cogenObsSummaryWithTitleAndConf: Cogen[ObsSummaryWithTitleAndConf] =
    Cogen[
      (Observation.Id,
       String,
       Option[String],
       ObsStatus,
       ObsActiveStatus,
       Duration,
       Option[BasicConfiguration]
      )
    ]
      .contramap(o =>
        (o.id,
         o.title,
         o.subtitle.map(_.value),
         o.status,
         o.activeStatus,
         o.duration,
         o.configuration
        )
      )

  implicit val arbObsSummaryWithConstraintsAndConf: Arbitrary[ObsSummaryWithConstraintsAndConf] =
    Arbitrary[ObsSummaryWithConstraintsAndConf] {
      for {
        id            <- arbitrary[Observation.Id]
        constraints   <- arbitrary[ConstraintsSummary]
        status        <- arbitrary[ObsStatus]
        activeStatus  <- arbitrary[ObsActiveStatus]
        duration      <- arbitrary[Duration]
        targets       <- arbitrary[Set[Target.Id]]
        configuration <- arbitrary[Option[BasicConfiguration]]
        vizTime       <- arbitrary[Option[Instant]]
        pa            <- arbitrary[Option[PosAngleConstraint]]
        wv            <- arbitrary[Option[Wavelength]]
      } yield ObsSummaryWithConstraintsAndConf(
        id,
        constraints,
        status,
        activeStatus,
        duration,
        targets,
        configuration,
        vizTime,
        pa,
        wv
      )
    }

  implicit val cogenObsSummaryWithConstraintsAndConf: Cogen[ObsSummaryWithConstraintsAndConf] =
    Cogen[
      (Observation.Id,
       ConstraintsSummary,
       ObsStatus,
       ObsActiveStatus,
       Duration,
       Option[BasicConfiguration],
       List[Target.Id],
       Option[Instant],
       Option[PosAngleConstraint],
       Option[Wavelength]
      )
    ]
      .contramap(o =>
        (o.id,
         o.constraints,
         o.status,
         o.activeStatus,
         o.duration,
         o.configuration,
         o.scienceTargetIds.toList,
         o.visualizationTime,
         o.posAngleConstraint,
         o.wavelength
        )
      )

}

object ArbObsSummary extends ArbObsSummary
