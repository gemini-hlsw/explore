// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import cats.Order.given
import eu.timepit.refined.scalacheck.numeric.given
import eu.timepit.refined.scalacheck.string.given
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.ObsSummary
import explore.model.ScienceRequirements
import lucuma.core.arb.ArbTime
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsStatus
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbWavelength.given
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Group
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Observation
import lucuma.core.model.ObservationValidation
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Target
import lucuma.core.model.TimingWindow
import lucuma.core.model.arb.ArbConstraintSet.given
import lucuma.core.model.arb.ArbObservationValidation.given
import lucuma.core.model.arb.ArbPosAngleConstraint.given
import lucuma.core.model.arb.ArbTimingWindow.given
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbGid.given
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.model.arb.ArbObservingMode
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

import java.time.Instant
import scala.collection.immutable.SortedSet

trait ArbObsSummary:
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
        scienceTargetIds    <- arbitrary[Set[Target.Id]]
        constraints         <- arbitrary[ConstraintSet]
        timingWindows       <- arbitrary[List[TimingWindow]]
        attachmentIds       <- arbitrary[Set[ObsAttachment.Id]]
        scienceRequirements <- arbitrary[ScienceRequirements]
        observingMode       <- arbitrary[Option[ObservingMode]]
        vizTime             <- arbitrary[Option[Instant]]
        posAngleConstraint  <- arbitrary[PosAngleConstraint]
        wavelength          <- arbitrary[Option[Wavelength]]
        groupId             <- arbitrary[Option[Group.Id]]
        groupIndex          <- arbitrary[NonNegShort]
        validations         <- arbitrary[List[ObservationValidation]]
        observerNotes       <- arbitrary[Option[NonEmptyString]]
      yield ObsSummary(
        id,
        title,
        subtitle,
        status,
        activeStatus,
        SortedSet.from(scienceTargetIds),
        constraints,
        timingWindows,
        SortedSet.from(attachmentIds),
        scienceRequirements,
        observingMode,
        vizTime,
        posAngleConstraint,
        wavelength,
        groupId,
        groupIndex,
        validations,
        observerNotes
      )
    )

  given Cogen[ObsSummary] =
    Cogen[
      (Observation.Id,
       String,
       Option[String],
       ObsStatus,
       ObsActiveStatus,
       List[Target.Id],
       ConstraintSet,
       List[TimingWindow],
       ScienceRequirements,
       Option[ObservingMode],
       Option[Instant],
       PosAngleConstraint,
       Option[Wavelength],
       Option[Group.Id],
       Short,
       List[ObservationValidation],
       Option[String]
      )
    ]
      .contramap(o =>
        (o.id,
         o.title,
         o.subtitle.map(_.value),
         o.status,
         o.activeStatus,
         o.scienceTargetIds.toList,
         o.constraints,
         o.timingWindows,
         o.scienceRequirements,
         o.observingMode,
         o.visualizationTime,
         o.posAngleConstraint,
         o.wavelength,
         o.groupId,
         o.groupIndex.value,
         o.validations,
         o.observerNotes.map(_.value)
        )
      )

object ArbObsSummary extends ArbObsSummary
