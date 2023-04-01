// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import eu.timepit.refined.scalacheck.string.given
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.ObsSummary
import lucuma.core.arb.ArbTime
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsStatus
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbWavelength.given
import lucuma.core.model.Observation
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Target
import lucuma.core.model.arb.ArbPosAngleConstraint.given
import lucuma.core.model.arb.ArbConstraintSet.given
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbGid.given
import lucuma.core.util.arb.ArbTimeSpan
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.arb.ArbBasicConfiguration
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

import java.time.Instant
import lucuma.core.model.ConstraintSet

trait ArbObsSummary:
  import ArbBasicConfiguration.given
  import ArbTimeSpan.given
  import ArbTime.given

  given Arbitrary[ObsSummary] =
    Arbitrary(
      for {
        id                 <- arbitrary[Observation.Id]
        title              <- arbitrary[String]
        subtitle           <- arbitrary[Option[NonEmptyString]]
        status             <- arbitrary[ObsStatus]
        activeStatus       <- arbitrary[ObsActiveStatus]
        executionTime      <- arbitrary[TimeSpan]
        scienceTargetIds   <- arbitrary[Set[Target.Id]]
        constraints        <- arbitrary[ConstraintSet]
        configuration      <- arbitrary[Option[BasicConfiguration]]
        vizTime            <- arbitrary[Option[Instant]]
        posAngleConstraint <- arbitrary[Option[PosAngleConstraint]]
        wavelength         <- arbitrary[Option[Wavelength]]
      } yield ObsSummary(
        id,
        title,
        subtitle,
        status,
        activeStatus,
        executionTime,
        scienceTargetIds,
        constraints,
        configuration,
        vizTime,
        posAngleConstraint,
        wavelength
      )
    )
  //       id:                 Observation.Id,
  // title:              String,
  // subtitle:           Option[NonEmptyString],
  // status:             ObsStatus,
  // activeStatus:       ObsActiveStatus,
  // executionTime:      TimeSpan,
  // scienceTargetIds:   Set[Target.Id],
  // constraints:        ConstraintSet,
  // configuration:      Option[BasicConfiguration],
  // visualizationTime:  Option[Instant],
  // posAngleConstraint: Option[PosAngleConstraint],
  // wavelength:         Option[Wavelength]

  given Cogen[ObsSummary] =
    Cogen[
      (Observation.Id,
       String,
       Option[String],
       ObsStatus,
       ObsActiveStatus,
       TimeSpan,
       List[Target.Id],
       ConstraintSet,
       Option[BasicConfiguration],
       Option[Instant],
       Option[PosAngleConstraint],
       Option[Wavelength]
      )
    ]
      .contramap(o =>
        (o.id,
         o.title,
         o.subtitle.map(_.value),
         o.status,
         o.activeStatus,
         o.executionTime,
         o.scienceTargetIds.toList,
         o.constraints,
         o.configuration,
         o.visualizationTime,
         o.posAngleConstraint,
         o.wavelength
        )
      )

object ArbObsSummary extends ArbObsSummary
