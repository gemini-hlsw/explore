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
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ScienceBand
import lucuma.core.math.Wavelength
import lucuma.core.model.Configuration
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ObsAttachment
import lucuma.core.model.ObservationValidation
import lucuma.core.model.ObservationWorkflow
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import lucuma.core.model.TimingWindow
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.longslit.*
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.odb.json.configurationrequest.query.given
import lucuma.odb.json.time.decoder.given
import lucuma.odb.json.wavelength.decoder.given
import lucuma.schemas.decoders.given
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.ObservingMode
import monocle.Focus
import monocle.Lens
import org.typelevel.cats.time.*

import java.time.Instant
import scala.collection.immutable.SortedSet

case class Observation(
  id:                  Observation.Id,
  title:               String,
  subtitle:            Option[NonEmptyString],
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
  observerNotes:       Option[NonEmptyString],
  calibrationRole:     Option[CalibrationRole],
  scienceBand:         Option[ScienceBand],
  configuration:       Option[Configuration],
  workflow:            ObservationWorkflow
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

  private val executedStates =
    Set(ObservationWorkflowState.Ongoing, ObservationWorkflowState.Completed)
  lazy val disabledStates    =
    Enumerated[
      ObservationWorkflowState
    ].all.toSet -- workflow.validTransitions.toSet - workflow.state

  inline def isCalibration: Boolean = calibrationRole.isDefined
  inline def isExecuted: Boolean    =
    executedStates.contains(workflow.state) ||
      workflow.validTransitions.exists(executedStates.contains)

  inline def configurationApplies(config: Configuration): Boolean =
    configuration.fold(false)(config.subsumes)

  // If an observation has a ConfigurationRequest* error, it is the only error they will have
  inline def hasNeedsApprovalError: Boolean =
    workflow.validationErrors.exists(ov =>
      ov.code === ObservationValidationCode.ConfigurationRequestNotRequested
    )

  inline def updateNeedsApprovalToPending: Observation =
    if (hasNeedsApprovalError)
      Observation.validationErrors.replace(List(ObservationValidation.configurationRequestPending))(
        this
      )
    else this

  def updateToPendingIfConfigurationApplies(config: Configuration): Observation =
    if (configurationApplies(config)) updateNeedsApprovalToPending
    else this

object Observation:
  type Id = lucuma.core.model.Observation.Id
  val Id = lucuma.core.model.Observation.Id

  val id                       = Focus[Observation](_.id)
  val title                    = Focus[Observation](_.title)
  val subtitle                 = Focus[Observation](_.subtitle)
  val scienceTargetIds         = Focus[Observation](_.scienceTargetIds)
  val selectedGSName           = Focus[Observation](_.selectedGSName)
  val constraints              = Focus[Observation](_.constraints)
  val timingWindows            = Focus[Observation](_.timingWindows)
  val attachmentIds            = Focus[Observation](_.attachmentIds)
  val scienceRequirements      = Focus[Observation](_.scienceRequirements)
  val observingMode            = Focus[Observation](_.observingMode)
  val observationTime          = Focus[Observation](_.observationTime)
  val observationDuration      = Focus[Observation](_.observationDuration)
  val posAngleConstraint       = Focus[Observation](_.posAngleConstraint)
  val wavelength               = Focus[Observation](_.wavelength)
  val observerNotes            = Focus[Observation](_.observerNotes)
  val calibrationRole          = Focus[Observation](_.calibrationRole)
  val scienceBand              = Focus[Observation](_.scienceBand)
  val configuration            = Focus[Observation](_.configuration)
  val workflow                 = Focus[Observation](_.workflow)
  val workflowState            = workflow.andThen(ObservationWorkflow.state)
  val workflowValidTransitions = workflow.andThen(ObservationWorkflow.validTransitions)
  val validationErrors         = workflow.andThen(ObservationWorkflow.validationErrors)

  // unlawful because it also updates the list of valid transitions, but
  // is needed for optimistically setting the state in the ObsBadge
  val unlawfulWorkflowState: Lens[Observation, ObservationWorkflowState] =
    Lens[Observation, ObservationWorkflowState](_.workflow.state)(newState =>
      o =>
        val oldState = o.workflow.state
        if (oldState === newState) o
        else
          val newAllowed = oldState +: o.workflow.validTransitions.filterNot(_ === newState)
          workflowState.replace(newState).andThen(workflowValidTransitions.replace(newAllowed))(o)
    )

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
      observerNotes       <- c.get[Option[NonEmptyString]]("observerNotes")
      calibrationRole     <- c.get[Option[CalibrationRole]]("calibrationRole")
      scienceBand         <- c.get[Option[ScienceBand]]("scienceBand")
      configuration       <- c.get[Configuration]("configuration").fold(_ => none.asRight, _.some.asRight)
      workflow            <- c.get[ObservationWorkflow]("workflow")
    } yield Observation(
      id,
      title,
      subtitle,
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
      observerNotes,
      calibrationRole,
      scienceBand,
      configuration,
      workflow
    )
  )
