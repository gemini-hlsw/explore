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
import explore.model.ObsSummaryWithTitleAndConf
import explore.model.ScienceModeBasic
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
  import ArbScienceMode._

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

  implicit val cogenObsSummaryWithConstraints: Cogen[ObsSummaryWithConstraints] =
    Cogen[
      (Observation.Id, ConstraintsSummary, ObsStatus, ObsActiveStatus, Duration, List[Target.Id])
    ]
      .contramap(o =>
        (o.id, o.constraints, o.status, o.activeStatus, o.duration, o.scienceTargetIds.toList)
      )

  implicit val arbObsSummaryWithTitleAndConstraints =
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

  implicit val arbObsSummaryWithTitleConstraintsAndConf =
    Arbitrary[ObsSummaryWithTitleConstraintsAndConf] {
      for {
        id           <- arbitrary[Observation.Id]
        title        <- arbitrary[String]
        subtitle     <- arbitrary[Option[NonEmptyString]]
        constraints  <- arbitrary[ConstraintsSummary]
        status       <- arbitrary[ObsStatus]
        activeStatus <- arbitrary[ObsActiveStatus]
        duration     <- arbitrary[Duration]
        mode         <- arbitrary[Option[ScienceModeBasic]]
      } yield ObsSummaryWithTitleConstraintsAndConf(
        id,
        title,
        subtitle,
        constraints,
        status,
        activeStatus,
        duration,
        mode
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
       Option[ScienceModeBasic]
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
         o.scienceMode
        )
      )

  implicit val arbObsSummaryWithTitleAndConf =
    Arbitrary[ObsSummaryWithTitleAndConf] {
      for {
        id           <- arbitrary[Observation.Id]
        title        <- arbitrary[String]
        subtitle     <- arbitrary[Option[NonEmptyString]]
        status       <- arbitrary[ObsStatus]
        activeStatus <- arbitrary[ObsActiveStatus]
        duration     <- arbitrary[Duration]
        mode         <- arbitrary[Option[ScienceModeBasic]]
      } yield ObsSummaryWithTitleAndConf(
        id,
        title,
        subtitle,
        status,
        activeStatus,
        duration,
        mode
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
       Option[ScienceModeBasic]
      )
    ]
      .contramap(o =>
        (o.id,
         o.title,
         o.subtitle.map(_.value),
         o.status,
         o.activeStatus,
         o.duration,
         o.scienceMode
        )
      )

  implicit val arbObsSummaryWithConstraintsAndConf =
    Arbitrary[ObsSummaryWithConstraintsAndConf] {
      for {
        id           <- arbitrary[Observation.Id]
        constraints  <- arbitrary[ConstraintsSummary]
        status       <- arbitrary[ObsStatus]
        activeStatus <- arbitrary[ObsActiveStatus]
        duration     <- arbitrary[Duration]
        targets      <- arbitrary[Set[Target.Id]]
        mode         <- arbitrary[Option[ScienceModeBasic]]
      } yield ObsSummaryWithConstraintsAndConf(
        id,
        constraints,
        status,
        activeStatus,
        duration,
        targets,
        mode
      )
    }

  implicit val cogenObsSummaryWithConstraintsAndConf: Cogen[ObsSummaryWithConstraintsAndConf] =
    Cogen[
      (Observation.Id,
       ConstraintsSummary,
       ObsStatus,
       ObsActiveStatus,
       Duration,
       Option[ScienceModeBasic],
       List[Target.Id]
      )
    ]
      .contramap(o =>
        (o.id,
         o.constraints,
         o.status,
         o.activeStatus,
         o.duration,
         o.scienceMode,
         o.scienceTargetIds.toList
        )
      )

}

object ArbObsSummary extends ArbObsSummary
