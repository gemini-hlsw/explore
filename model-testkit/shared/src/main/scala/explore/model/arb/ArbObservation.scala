// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import cats.Order.given
import eu.timepit.refined.scalacheck.string.given
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.Observation
import explore.model.ScienceRequirements
import lucuma.core.arb.ArbTime
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsStatus
import lucuma.core.enums.ScienceBand
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbWavelength.given
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ObsAttachment
import lucuma.core.model.ObservationValidation
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Target
import lucuma.core.model.TimingWindow
import lucuma.core.model.arb.ArbConstraintSet.given
import lucuma.core.model.arb.ArbObservationValidation.given
import lucuma.core.model.arb.ArbPosAngleConstraint.given
import lucuma.core.model.arb.ArbTimingWindow.given
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbGid.given
import lucuma.core.util.arb.ArbTimeSpan.given
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.model.arb.ArbObservingMode
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

import java.time.Instant
import scala.collection.immutable.SortedSet
import lucuma.core.enums.CalibrationRole

trait ArbObservation:
  import ArbTime.given
  import ArbScienceRequirements.given
  import ArbObservingMode.given

  given Arbitrary[Observation] =
    Arbitrary(
      for
        id                  <- arbitrary[Observation.Id]
        title               <- arbitrary[String]
        subtitle            <- arbitrary[Option[NonEmptyString]]
        status              <- arbitrary[ObsStatus]
        activeStatus        <- arbitrary[ObsActiveStatus]
        scienceTargetIds    <- arbitrary[Set[Target.Id]]
        selectedGSName      <- arbitrary[Option[NonEmptyString]]
        constraints         <- arbitrary[ConstraintSet]
        timingWindows       <- arbitrary[List[TimingWindow]]
        attachmentIds       <- arbitrary[Set[ObsAttachment.Id]]
        scienceRequirements <- arbitrary[ScienceRequirements]
        observingMode       <- arbitrary[Option[ObservingMode]]
        vizTime             <- arbitrary[Option[Instant]]
        vizDuration         <- arbitrary[Option[TimeSpan]]
        posAngleConstraint  <- arbitrary[PosAngleConstraint]
        wavelength          <- arbitrary[Option[Wavelength]]
        validations         <- arbitrary[List[ObservationValidation]]
        observerNotes       <- arbitrary[Option[NonEmptyString]]
        calibrationRole     <- arbitrary[Option[CalibrationRole]]
        scienceBand         <- arbitrary[Option[ScienceBand]]
      yield Observation(
        id,
        title,
        subtitle,
        status,
        activeStatus,
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
        wavelength,
        validations,
        observerNotes,
        calibrationRole,
        scienceBand
      )
    )

  given Cogen[Observation] =
    Cogen[
      (Observation.Id,
       String,
       Option[String],
       ObsStatus,
       ObsActiveStatus,
       List[Target.Id],
       Option[String],
       ConstraintSet,
       List[TimingWindow],
       ScienceRequirements,
       Option[ObservingMode],
       Option[Instant],
       Option[TimeSpan],
       PosAngleConstraint,
       Option[Wavelength],
       List[ObservationValidation],
       Option[String],
       Option[CalibrationRole],
       Option[ScienceBand]
      )
    ]
      .contramap(o =>
        (o.id,
         o.title,
         o.subtitle.map(_.value),
         o.status,
         o.activeStatus,
         o.scienceTargetIds.toList,
         o.selectedGSName.map(_.value),
         o.constraints,
         o.timingWindows,
         o.scienceRequirements,
         o.observingMode,
         o.observationTime,
         o.observationDuration,
         o.posAngleConstraint,
         o.wavelength,
         o.validations,
         o.observerNotes.map(_.value),
         o.calibrationRole,
         o.scienceBand
        )
      )

object ArbObservation extends ArbObservation
