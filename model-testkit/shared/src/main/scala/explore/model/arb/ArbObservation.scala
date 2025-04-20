// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import cats.Order.given
import eu.timepit.refined.scalacheck.string.given
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.Observation
import explore.model.ScienceRequirements
import lucuma.core.arb.ArbTime
import lucuma.core.enums.ScienceBand
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbWavelength.given
import lucuma.core.model.Attachment
import lucuma.core.model.Configuration
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ObservationReference
import lucuma.core.model.ObservationValidation
import lucuma.core.model.ObservationWorkflow
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Target
import lucuma.core.model.TimingWindow
import lucuma.core.model.arb.ArbConfiguration.given
import lucuma.core.model.arb.ArbConstraintSet.given
import lucuma.core.model.arb.ArbObservationReference.given
import lucuma.core.model.arb.ArbObservationValidation.given
import lucuma.core.model.arb.ArbObservationWorkflow.given
import lucuma.core.model.arb.ArbPosAngleConstraint.given
import lucuma.core.model.arb.ArbTimingWindow.given
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbGid.given
import lucuma.core.util.arb.ArbTimeSpan.given
import lucuma.schemas.model.CentralWavelength
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.model.arb.ArbObservingMode
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

import java.time.Instant
import scala.collection.immutable.SortedSet
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.Group
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.scalacheck.numeric.given

trait ArbObservation:
  import ArbTime.given
  import ArbScienceRequirements.given
  import ArbObservingMode.given

  given Arbitrary[Observation] =
    Arbitrary(
      for
        id                  <- arbitrary[Observation.Id]
        reference           <- arbitrary[Option[ObservationReference]]
        title               <- arbitrary[String]
        subtitle            <- arbitrary[Option[NonEmptyString]]
        scienceTargetIds    <- arbitrary[Set[Target.Id]]
        selectedGSName      <- arbitrary[Option[NonEmptyString]]
        constraints         <- arbitrary[ConstraintSet]
        timingWindows       <- arbitrary[List[TimingWindow]]
        attachmentIds       <- arbitrary[Set[Attachment.Id]]
        scienceRequirements <- arbitrary[ScienceRequirements]
        observingMode       <- arbitrary[Option[ObservingMode]]
        vizTime             <- arbitrary[Option[Instant]]
        vizDuration         <- arbitrary[Option[TimeSpan]]
        posAngleConstraint  <- arbitrary[PosAngleConstraint]
        centralWavelength   <- arbitrary[Option[Wavelength]].map(_.map(CentralWavelength.apply))
        validations         <- arbitrary[List[ObservationValidation]]
        observerNotes       <- arbitrary[Option[NonEmptyString]]
        calibrationRole     <- arbitrary[Option[CalibrationRole]]
        scienceBand         <- arbitrary[Option[ScienceBand]]
        configuration       <- arbitrary[Option[Configuration]]
        workflow            <- arbitrary[ObservationWorkflow]
        groupId             <- arbitrary[Option[Group.Id]]
        groupIndex          <- arbitrary[NonNegShort]
      yield Observation(
        id,
        reference,
        title,
        subtitle,
        SortedSet.from(scienceTargetIds),
        selectedGSName,
        constraints,
        timingWindows,
        SortedSet.from(attachmentIds),
        scienceRequirements,
        observingMode,
        vizTime,
        vizDuration,
        posAngleConstraint,
        centralWavelength,
        observerNotes,
        calibrationRole,
        scienceBand,
        configuration,
        workflow,
        groupId,
        groupIndex
      )
    )

  given Cogen[Observation] =
    Cogen[
      (Observation.Id,
       Option[ObservationReference],
       String,
       Option[String],
       List[Target.Id],
       Option[String],
       ConstraintSet,
       List[TimingWindow],
       SortedSet[Attachment.Id],
       ScienceRequirements,
       Option[ObservingMode],
       Option[Instant],
       Option[TimeSpan],
       PosAngleConstraint,
       Option[Wavelength],
       Option[String],
       Option[CalibrationRole],
       Option[ScienceBand],
       Option[Configuration],
       ObservationWorkflow,
       Option[Group.Id],
       Short
      )
    ]
      .contramap(o =>
        (o.id,
         o.reference,
         o.title,
         o.subtitle.map(_.value),
         o.scienceTargetIds.toList,
         o.selectedGSName.map(_.value),
         o.constraints,
         o.timingWindows,
         o.attachmentIds,
         o.scienceRequirements,
         o.observingMode,
         o.observationTime,
         o.observationDuration,
         o.posAngleConstraint,
         o.centralWavelength.map(_.value),
         o.observerNotes.map(_.value),
         o.calibrationRole,
         o.scienceBand,
         o.configuration,
         o.workflow,
         o.groupId,
         o.groupIndex.value
        )
      )

object ArbObservation extends ArbObservation
