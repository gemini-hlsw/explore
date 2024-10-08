// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.Order.given
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.syntax.all.*
import explore.modes.GmosSpectroscopyOverrides
import explore.modes.InstrumentOverrides
import io.circe.Decoder
import io.circe.generic.semiauto.*
import io.circe.refined.given
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsStatus
import lucuma.core.enums.ScienceBand
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ObsAttachment
import lucuma.core.model.ObservationValidation
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import lucuma.core.model.TimingWindow
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.longslit.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.odb.json.time.decoder.given
import lucuma.odb.json.wavelength.decoder.given
import lucuma.schemas.decoders.given
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.ObservingMode
import monocle.Focus
import org.typelevel.cats.time.*

import java.time.Instant
import scala.collection.immutable.SortedSet

case class Observation(
  id:                  Observation.Id,
  title:               String,
  subtitle:            Option[NonEmptyString],
  status:              ObsStatus,
  activeStatus:        ObsActiveStatus,
  scienceTargetIds:    AsterismIds,
  selectedGSName:      Option[NonEmptyString],
  constraints:         ConstraintSet,
  timingWindows:       List[TimingWindow],
  attachmentIds:       SortedSet[ObsAttachment.Id],
  scienceRequirements: ScienceRequirements,
  observingMode:       Option[ObservingMode],
  observationTime:     Option[Instant],
  observationDuration: Option[TimeSpan],
  posAngleConstraint:  PosAngleConstraint,
  wavelength:          Option[Wavelength],
  validations:         List[ObservationValidation],
  observerNotes:       Option[NonEmptyString],
  calibrationRole:     Option[CalibrationRole],
  scienceBand:         Option[ScienceBand]
) derives Eq:
  lazy val basicConfiguration: Option[BasicConfiguration] =
    observingMode.map(_.toBasicConfiguration)

  lazy val configurationSummary: Option[String] =
    basicConfiguration match
      case Some(BasicConfiguration.GmosNorthLongSlit(grating, _, fpu, _)) =>
        s"GMOS-N ${grating.shortName} ${fpu.shortName}".some
      case Some(BasicConfiguration.GmosSouthLongSlit(grating, _, fpu, _)) =>
        s"GMOS-S ${grating.shortName} ${fpu.shortName}".some
      case _                                                              =>
        none

  val needsAGS: Boolean = calibrationRole.forall(_.needsAGS)

  // For multiple targets, we take the smallest binning for each axis.
  // https://docs.google.com/document/d/1P8_pXLRVomUSvofyVkAniOyGROcAtiJ7EMYt9wWXB0o/edit?disco=AAAA32SmtD4
  private def asterismBinning(
    bs: NonEmptyList[(GmosXBinning, GmosYBinning)]
  ): (GmosXBinning, GmosYBinning) =
    (bs.map(_._1).minimumBy(_.count), bs.map(_._2).minimumBy(_.count))

  private def profiles(targets: TargetList): Option[NonEmptyList[SourceProfile]] =
    NonEmptyList.fromList:
      scienceTargetIds.toList.map(targets.get).flattenOption.map(_.sourceProfile)

  def toModeOverride(targets: TargetList): Option[InstrumentOverrides] =
    observingMode.flatMap:
      case ObservingMode.GmosNorthLongSlit(
            _,
            grating,
            _,
            _,
            _,
            fpu,
            _,
            centralWavelength,
            _,
            explicitXBinning,
            _,
            explicitYBinning,
            defaultAmpReadMode,
            explicitAmpReadMode,
            defaultAmpGain,
            explicitAmpGain,
            _,
            explicitRoi,
            _,
            _,
            _,
            _
          ) =>
        profiles(targets).map: ps =>
          val (defaultXBinning, defaultYBinning) =
            if (fpu.isIFU) (GmosXBinning.One, GmosYBinning.One)
            else asterismBinning(ps.map(northBinning(fpu, _, constraints.imageQuality, grating)))

          GmosSpectroscopyOverrides(
            centralWavelength,
            GmosCcdMode(
              explicitXBinning.getOrElse(defaultXBinning),
              explicitYBinning.getOrElse(defaultYBinning),
              GmosAmpCount.Twelve,
              explicitAmpGain.getOrElse(defaultAmpGain),
              explicitAmpReadMode.getOrElse(defaultAmpReadMode)
            ).some,
            explicitRoi
          )
      case ObservingMode.GmosSouthLongSlit(
            _,
            grating,
            _,
            _,
            _,
            fpu,
            _,
            centralWavelength,
            _,
            explicitXBinning,
            _,
            explicitYBinning,
            defaultAmpReadMode,
            explicitAmpReadMode,
            defaultAmpGain,
            explicitAmpGain,
            _,
            explicitRoi,
            _,
            _,
            _,
            _
          ) =>
        profiles(targets).map: ps =>
          val (defaultXBinning, defaultYBinning) =
            if (fpu.isIFU) (GmosXBinning.One, GmosYBinning.One)
            else asterismBinning(ps.map(southBinning(fpu, _, constraints.imageQuality, grating)))

          GmosSpectroscopyOverrides(
            centralWavelength,
            GmosCcdMode(
              explicitXBinning.getOrElse(defaultXBinning),
              explicitYBinning.getOrElse(defaultYBinning),
              GmosAmpCount.Twelve,
              explicitAmpGain.getOrElse(defaultAmpGain),
              explicitAmpReadMode.getOrElse(defaultAmpReadMode)
            ).some,
            explicitRoi
          )

  lazy val constraintsSummary: String =
    s"${constraints.imageQuality.label} ${constraints.cloudExtinction.label} ${constraints.skyBackground.label} ${constraints.waterVapor.label}"

  inline def isCalibration: Boolean = calibrationRole.isDefined
  inline def isExecuted: Boolean    = status >= ObsStatus.Ongoing

object Observation:
  type Id = lucuma.core.model.Observation.Id
  val Id = lucuma.core.model.Observation.Id

  val id                  = Focus[Observation](_.id)
  val title               = Focus[Observation](_.title)
  val subtitle            = Focus[Observation](_.subtitle)
  val status              = Focus[Observation](_.status)
  val activeStatus        = Focus[Observation](_.activeStatus)
  val scienceTargetIds    = Focus[Observation](_.scienceTargetIds)
  val selectedGSName      = Focus[Observation](_.selectedGSName)
  val constraints         = Focus[Observation](_.constraints)
  val timingWindows       = Focus[Observation](_.timingWindows)
  val attachmentIds       = Focus[Observation](_.attachmentIds)
  val scienceRequirements = Focus[Observation](_.scienceRequirements)
  val observingMode       = Focus[Observation](_.observingMode)
  val observationTime     = Focus[Observation](_.observationTime)
  val observationDuration = Focus[Observation](_.observationDuration)
  val posAngleConstraint  = Focus[Observation](_.posAngleConstraint)
  val wavelength          = Focus[Observation](_.wavelength)
  val validations         = Focus[Observation](_.validations)
  val observerNotes       = Focus[Observation](_.observerNotes)
  val calibrationRole     = Focus[Observation](_.calibrationRole)
  val scienceBand         = Focus[Observation](_.scienceBand)

  private case class TargetIdWrapper(id: Target.Id)
  private object TargetIdWrapper:
    given Decoder[TargetIdWrapper] = deriveDecoder

  private case class AttachmentIdWrapper(id: ObsAttachment.Id)
  private object AttachmentIdWrapper:
    given Decoder[AttachmentIdWrapper] = deriveDecoder

  given Decoder[Observation] = Decoder.instance(c =>
    for {
      id                  <- c.get[Observation.Id]("id")
      title               <- c.get[String]("title")
      subtitle            <- c.get[Option[NonEmptyString]]("subtitle")
      status              <- c.get[ObsStatus]("status")
      activeStatus        <- c.get[ObsActiveStatus]("activeStatus")
      scienceTargetIds    <- c.downField("targetEnvironment").get[List[TargetIdWrapper]]("asterism")
      selectedGSName      <- c.downField("targetEnvironment")
                               .downField("guideTargetName")
                               .as[Option[NonEmptyString]]
      constraints         <- c.get[ConstraintSet]("constraintSet")
      timingWindows       <- c.get[List[TimingWindow]]("timingWindows")
      attachmentIds       <- c.get[List[AttachmentIdWrapper]]("obsAttachments")
      scienceRequirements <- c.get[ScienceRequirements]("scienceRequirements")
      observingMode       <- c.get[Option[ObservingMode]]("observingMode")
      observationTime     <- c.get[Option[Timestamp]]("observationTime")
      observationDur      <- c.get[Option[TimeSpan]]("observationDuration")
      posAngleConstraint  <- c.get[PosAngleConstraint]("posAngleConstraint")
      wavelength          <- c.downField("scienceRequirements")
                               .downField("spectroscopy")
                               .get[Option[Wavelength]]("wavelength")
      validations         <- c.get[List[ObservationValidation]]("validations")
      observerNotes       <- c.get[Option[NonEmptyString]]("observerNotes")
      calibrationRole     <- c.get[Option[CalibrationRole]]("calibrationRole")
      scienceBand         <- c.get[Option[ScienceBand]]("scienceBand")
    } yield Observation(
      id,
      title,
      subtitle,
      status,
      activeStatus,
      SortedSet.from(scienceTargetIds.map(_.id)),
      selectedGSName,
      constraints,
      timingWindows,
      SortedSet.from(attachmentIds.map(_.id)),
      scienceRequirements,
      observingMode,
      observationTime.map(_.toInstant),
      observationDur,
      posAngleConstraint,
      wavelength,
      validations,
      observerNotes,
      calibrationRole,
      scienceBand
    )
  )
