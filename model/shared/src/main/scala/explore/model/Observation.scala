// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.Order.given
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import explore.givens.given
import explore.model.syntax.all.*
import explore.modes.InstrumentOverrides
import explore.modes.ItcInstrumentConfig
import io.circe.Decoder
import io.circe.refined.given
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.Site
import lucuma.core.math.Wavelength
import lucuma.core.model.Attachment
import lucuma.core.model.Configuration
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ObjectTracking
import lucuma.core.model.ObservationReference
import lucuma.core.model.ObservationValidation
import lucuma.core.model.ObservationWorkflow
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import lucuma.core.model.TimingWindow
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.util.CalculatedValue
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.odb.json.configurationrequest.query.given
import lucuma.odb.json.time.decoder.given
import lucuma.schemas.decoders.given
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.CentralWavelength
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.model.ObservingMode.given
import monocle.Focus
import monocle.Lens
import org.typelevel.cats.time.*

import java.time.Instant
import scala.collection.immutable.SortedSet

case class Observation(
  id:                      Observation.Id,
  reference:               Option[ObservationReference],
  title:                   String,
  subtitle:                Option[NonEmptyString],
  scienceTargetIds:        AsterismIds,
  selectedGSName:          Option[NonEmptyString],
  constraints:             ConstraintSet,
  timingWindows:           List[TimingWindow],
  attachmentIds:           SortedSet[Attachment.Id],
  scienceRequirements:     ScienceRequirements,
  observingMode:           Option[ObservingMode],
  observationTime:         Option[Instant],
  observationDuration:     Option[TimeSpan],
  posAngleConstraint:      PosAngleConstraint,
  centralWavelength:       Option[CentralWavelength],
  observerNotes:           Option[NonEmptyString],
  calibrationRole:         Option[CalibrationRole],
  scienceBand:             Option[ScienceBand],
  configuration:           Option[Configuration],
  configurationRequestIds: SortedSet[ConfigurationRequest.Id],
  workflow:                CalculatedValue[ObservationWorkflow],
  groupId:                 Option[Group.Id],
  groupIndex:              NonNegShort,
  execution:               Execution
) derives Eq:
  lazy val basicConfiguration: Option[BasicConfiguration] =
    observingMode.map(_.toBasicConfiguration)

  val site: Option[Site] = observingMode.map(_.siteFor)

  lazy val observingModeSummary: Option[ObservingModeSummary] =
    observingMode.map(ObservingModeSummary.fromObservingMode)

  private def profiles(targets: TargetList): Option[NonEmptyList[SourceProfile]] =
    NonEmptyList.fromList:
      scienceTargetIds.toList.map(targets.get).flattenOption.map(_.sourceProfile)

  private def applyGmosCcdModesOverrides(
    explicitXBinning:    Option[GmosXBinning],
    explicitYBinning:    Option[GmosYBinning],
    explicitAmpReadMode: Option[GmosAmpReadMode],
    explicitAmpGain:     Option[GmosAmpGain]
  ): GmosCcdMode => GmosCcdMode =
    List(
      explicitXBinning.foldMap(GmosCcdMode.xBin.replace),
      explicitYBinning.foldMap(GmosCcdMode.yBin.replace),
      explicitAmpReadMode.foldMap(GmosCcdMode.ampReadMode.replace),
      explicitAmpGain.foldMap(GmosCcdMode.ampGain.replace)
    ).reduce(_ >>> _)

  // Computes mode parameters locally, for quick invocation of ITC.
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
            _,
            explicitAmpReadMode,
            _,
            explicitAmpGain,
            defaultRoi,
            explicitRoi,
            _,
            _,
            _,
            _
          ) =>
        profiles(targets).map: ps =>
          val defaultMode: GmosCcdMode =
            GmosCcdMode.Default.Longslit.gmosNorth(
              ps,
              fpu,
              grating,
              constraints.imageQuality.toImageQuality
            )

          val mode: GmosCcdMode =
            applyGmosCcdModesOverrides(
              explicitXBinning,
              explicitYBinning,
              explicitAmpReadMode,
              explicitAmpGain
            )(defaultMode)

          InstrumentOverrides.GmosSpectroscopy(
            centralWavelength,
            mode,
            explicitRoi.getOrElse(defaultRoi)
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
            _,
            explicitAmpReadMode,
            _,
            explicitAmpGain,
            defaultRoi,
            explicitRoi,
            _,
            _,
            _,
            _
          ) =>
        profiles(targets).map: ps =>
          val defaultMode: GmosCcdMode =
            GmosCcdMode.Default.Longslit.gmosSouth(
              ps,
              fpu,
              grating,
              constraints.imageQuality.toImageQuality
            )

          val mode: GmosCcdMode = applyGmosCcdModesOverrides(
            explicitXBinning,
            explicitYBinning,
            explicitAmpReadMode,
            explicitAmpGain
          )(defaultMode)

          InstrumentOverrides.GmosSpectroscopy(
            centralWavelength,
            mode,
            explicitRoi.getOrElse(defaultRoi)
          )
      case _: ObservingMode.Flamingos2LongSlit =>
        InstrumentOverrides.Flamingos2Spectroscopy().some
      case _: ObservingMode.GmosNorthImaging   =>
        InstrumentOverrides.GmosImaging().some
      case _: ObservingMode.GmosSouthImaging   =>
        InstrumentOverrides.GmosImaging().some

  // Imaging modes can return multiple configs due to multiple filters.
  def toInstrumentConfig(targets: TargetList): List[ItcInstrumentConfig] =
    import ObservingMode.*
    (toModeOverride(targets), observingMode)
      .mapN:
        case (o @ InstrumentOverrides.GmosSpectroscopy(_, _, _), n: GmosNorthLongSlit) =>
          List(ItcInstrumentConfig.GmosNorthSpectroscopy(n.grating, n.fpu, n.filter, o.some))
        case (o @ InstrumentOverrides.GmosImaging(), n: GmosNorthImaging)              =>
          n.filters.toList.map(ItcInstrumentConfig.GmosNorthImaging(_, o.some))
        case (o @ InstrumentOverrides.GmosImaging(), n: GmosSouthImaging)              =>
          n.filters.toList.map(ItcInstrumentConfig.GmosSouthImaging(_, o.some))
        case (o @ InstrumentOverrides.GmosSpectroscopy(_, _, _), s: GmosSouthLongSlit) =>
          List(ItcInstrumentConfig.GmosSouthSpectroscopy(s.grating, s.fpu, s.filter, o.some))
        case (_, f: ObservingMode.Flamingos2LongSlit)                                  =>
          List(ItcInstrumentConfig.Flamingos2Spectroscopy(f.disperser, f.filter, f.fpu))
        case _                                                                         =>
          List.empty
      .getOrElse(List.empty)

  lazy val constraintsSummary: String =
    s"${constraints.imageQuality.toImageQuality.label} ${constraints.cloudExtinction.toCloudExtinction.label}" +
      s" ${constraints.skyBackground.label} ${constraints.waterVapor.label}"

  lazy val disabledStates =
    Enumerated[
      ObservationWorkflowState
    ].all.toSet -- workflow.value.validTransitions.toSet - workflow.value.state

  val isInactive = workflow.value.state === ObservationWorkflowState.Inactive

  inline def isCalibration: Boolean = calibrationRole.isDefined
  lazy val isOngoing                =
    workflow.value.state === ObservationWorkflowState.Ongoing ||
      (workflow.value.state === ObservationWorkflowState.Inactive &&
        workflow.value.validTransitions.contains(ObservationWorkflowState.Ongoing))
  lazy val isCompleted              = workflow.value.state === ObservationWorkflowState.Completed
  lazy val isExecuted: Boolean      = isOngoing || isCompleted

  inline def newConfigurationRequestApplies(config: Configuration): Boolean =
    (hasNotRequestedCode || hasDeniedValidationCode) &&
      configuration.fold(false)(config.subsumes)

  inline def hasValidationErrors: Boolean =
    !workflow.value.validationErrors.isEmpty

  inline def hasValidationCode(code: ObservationValidationCode): Boolean =
    workflow.value.validationErrors.exists(_.code === code)

  // If an observation has a ConfigurationRequest* error, it is the only error they will have
  inline def hasPendingRequestCode: Boolean =
    hasValidationCode(ObservationValidationCode.ConfigurationRequestPending)

  inline def hasNotRequestedCode: Boolean =
    hasValidationCode(ObservationValidationCode.ConfigurationRequestNotRequested)

  inline def hasDeniedValidationCode: Boolean =
    hasValidationCode(ObservationValidationCode.ConfigurationRequestDenied)

  // if it has any of the ConfigurationRequest* errors. We filter these out of the validations table.
  inline def hasConfigurationRequestError: Boolean =
    hasPendingRequestCode || hasNotRequestedCode || hasDeniedValidationCode

  // update the validation status to pending and add the configuration request id
  inline def updateToPending(crId: ConfigurationRequest.Id): Observation =
    Observation.validationErrors
      .replace(List(ObservationValidation.configurationRequestPending))
      .andThen(Observation.configurationRequestIds.modify(_ + crId))(
        this
      )

  def updateToPendingIfConfigurationApplies(request: ConfigurationRequest): Observation =
    if (newConfigurationRequestApplies(request.configuration)) updateToPending(request.id)
    else this

  def asterismTracking(allTargets: TargetList): Option[ObjectTracking] =
    NonEmptyList
      .fromList:
        scienceTargetIds.toList
          .map(id => allTargets.get(id))
          .flattenOption
      .flatMap(ObjectTracking.fromAsterism(_))

  def hasTargetOfOpportunity(allTargets: TargetList): Boolean =
    scienceTargetIds.toList
      .map(id => allTargets.get(id).flatMap(Target.opportunity.getOption))
      .flattenOption
      .nonEmpty

  def needsAGS(allTargets: TargetList): Boolean =
    calibrationRole.forall(_.needsAGS) && hasTargetOfOpportunity(allTargets)

object Observation:
  type Id = lucuma.core.model.Observation.Id
  val Id = lucuma.core.model.Observation.Id

  val id                       = Focus[Observation](_.id)
  val reference                = Focus[Observation](_.reference)
  val title                    = Focus[Observation](_.title)
  val subtitle                 = Focus[Observation](_.subtitle)
  val scienceTargetIds         = Focus[Observation](_.scienceTargetIds)
  val selectedGSName           = Focus[Observation](_.selectedGSName)
  val constraints              = Focus[Observation](_.constraints)
  val centralWavelength        = Focus[Observation](_.centralWavelength)
  val timingWindows            = Focus[Observation](_.timingWindows)
  val attachmentIds            = Focus[Observation](_.attachmentIds)
  val scienceRequirements      = Focus[Observation](_.scienceRequirements)
  val observingMode            = Focus[Observation](_.observingMode)
  val observationTime          = Focus[Observation](_.observationTime)
  val observationDuration      = Focus[Observation](_.observationDuration)
  val posAngleConstraint       = Focus[Observation](_.posAngleConstraint)
  val observerNotes            = Focus[Observation](_.observerNotes)
  val calibrationRole          = Focus[Observation](_.calibrationRole)
  val scienceBand              = Focus[Observation](_.scienceBand)
  val configuration            = Focus[Observation](_.configuration)
  val configurationRequestIds  = Focus[Observation](_.configurationRequestIds)
  val workflow                 = Focus[Observation](_.workflow)
  val workflowState            = workflow.andThen(CalculatedValue.value).andThen(ObservationWorkflow.state)
  val workflowValidTransitions =
    workflow.andThen(CalculatedValue.value).andThen(ObservationWorkflow.validTransitions)
  val validationErrors         =
    workflow.andThen(CalculatedValue.value).andThen(ObservationWorkflow.validationErrors)
  val groupId                  = Focus[Observation](_.groupId)
  val groupIndex               = Focus[Observation](_.groupIndex)
  val execution                = Focus[Observation](_.execution)

  // unlawful because it also updates the list of valid transitions, but
  // is needed for optimistically setting the state in the ObsBadge
  val unlawfulWorkflowState: Lens[Observation, ObservationWorkflowState] =
    Lens[Observation, ObservationWorkflowState](_.workflow.value.state)(newState =>
      o =>
        val oldState = o.workflow.value.state
        if (oldState === newState) o
        else
          val newAllowed = oldState +: o.workflow.value.validTransitions.filterNot(_ === newState)
          workflowState.replace(newState).andThen(workflowValidTransitions.replace(newAllowed))(o)
    )

  private case class TargetIdWrapper(id: Target.Id) derives Decoder
  private case class AttachmentIdWrapper(id: Attachment.Id) derives Decoder
  private case class ConfigurationRequestIdWrapper(id: ConfigurationRequest.Id) derives Decoder

  given Decoder[Observation] = Decoder.instance(c =>
    for {
      id                  <- c.get[Observation.Id]("id")
      reference           <- c.downField("reference")
                               .downField("label")
                               .success
                               .traverse(_.as[Option[ObservationReference]])
      title               <- c.get[String]("title")
      subtitle            <- c.get[Option[NonEmptyString]]("subtitle")
      scienceTargetIds    <- c.downField("targetEnvironment").get[List[TargetIdWrapper]]("asterism")
      selectedGSName      <- c.downField("targetEnvironment")
                               .downField("guideTargetName")
                               .as[Option[NonEmptyString]]
      constraints         <- c.get[ConstraintSet]("constraintSet")
      timingWindows       <- c.get[List[TimingWindow]]("timingWindows")
      attachmentIds       <- c.get[List[AttachmentIdWrapper]]("attachments")
      scienceRequirements <- c.get[ScienceRequirements]("scienceRequirements")
      observingMode       <- c.get[Option[ObservingMode]]("observingMode")
      observationTime     <- c.get[Option[Timestamp]]("observationTime")
      observationDur      <- c.get[Option[TimeSpan]]("observationDuration")
      posAngleConstraint  <- c.get[PosAngleConstraint]("posAngleConstraint")
      observerNotes       <- c.get[Option[NonEmptyString]]("observerNotes")
      calibrationRole     <- c.get[Option[CalibrationRole]]("calibrationRole")
      scienceBand         <- c.get[Option[ScienceBand]]("scienceBand")
      configuration       <- c.get[Configuration]("configuration").fold(_ => none.asRight, _.some.asRight)
      crIds               <- c.get[List[ConfigurationRequestIdWrapper]]("configurationRequests")
      workflow            <- c.get[CalculatedValue[ObservationWorkflow]]("workflow")
      groupId             <- c.get[Option[Group.Id]]("groupId")
      groupIndex          <- c.get[NonNegShort]("groupIndex")
      execution           <- c.get[Execution]("execution")
    } yield Observation(
      id,
      reference.flatten,
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
      ScienceRequirements.spectroscopy
        .getOption(scienceRequirements)
        .flatMap(_.wavelength)
        .map(CentralWavelength(_)),
      observerNotes,
      calibrationRole,
      scienceBand,
      configuration,
      SortedSet.from(crIds.map(_.id)),
      workflow,
      groupId,
      groupIndex,
      execution
    )
  )
