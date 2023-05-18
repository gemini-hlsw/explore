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
import lucuma.core.model.TimingWindow
import lucuma.core.model.arb.ArbPosAngleConstraint.given
import lucuma.core.model.arb.ArbConstraintSet.given
import lucuma.core.model.arb.ArbTimingWindow.given
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbGid.given
import lucuma.core.util.arb.ArbTimeSpan
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.arb.ArbBasicConfiguration
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import cats.Order.given

import java.time.Instant
import lucuma.core.model.ConstraintSet
import scala.collection.immutable.SortedSet
import explore.model.ObsSummary.scienceRequirements
import explore.model.ObsSummary.observingMode
import explore.model.ScienceRequirements
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.model.arb.ArbObservingMode

trait ArbObsSummary:
  import ArbBasicConfiguration.given
  import ArbTimeSpan.given
  import ArbTime.given
  import ArbScienceRequirements.given
  import ArbObservingMode.given

  given Arbitrary[ObsSummary] =
    Arbitrary(
      for
        id                  <- arbitrary[Observation.Id]
        title               <- arbitrary[String]
        subtitle            <- arbitrary[Option[NonEmptyString]]
        status              <- arbitrary[ObsStatus]
        activeStatus        <- arbitrary[ObsActiveStatus]
        executionTime       <- arbitrary[TimeSpan]
        scienceTargetIds    <- arbitrary[Set[Target.Id]]
        constraints         <- arbitrary[ConstraintSet]
        timingWindows       <- arbitrary[List[TimingWindow]]
        scienceRequirements <- arbitrary[ScienceRequirements]
        observingMode       <- arbitrary[Option[ObservingMode]]
        vizTime             <- arbitrary[Option[Instant]]
        posAngleConstraint  <- arbitrary[PosAngleConstraint]
        wavelength          <- arbitrary[Option[Wavelength]]
      yield ObsSummary(
        id,
        title,
        subtitle,
        status,
        activeStatus,
        executionTime,
        SortedSet.from(scienceTargetIds),
        constraints,
        timingWindows,
        scienceRequirements,
        observingMode,
        vizTime,
        posAngleConstraint,
        wavelength
      )
    )

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
       List[TimingWindow],
       ScienceRequirements,
       Option[ObservingMode],
       Option[Instant],
       PosAngleConstraint,
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
         o.timingWindows,
         o.scienceRequirements,
         o.observingMode,
         o.visualizationTime,
         o.posAngleConstraint,
         o.wavelength
        )
      )

object ArbObsSummary extends ArbObsSummary
