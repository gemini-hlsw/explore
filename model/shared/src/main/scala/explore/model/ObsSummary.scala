// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.Order.given
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import explore.modes.GmosSpectroscopyOverrides
import explore.modes.InstrumentOverrides
import io.circe.Decoder
import io.circe.generic.semiauto.*
import io.circe.refined.given
import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsStatus
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Group
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Observation
import lucuma.core.model.ObservationValidation
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Target
import lucuma.core.model.TimingWindow
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.util.Timestamp
import lucuma.odb.json.wavelength.decoder.given
import lucuma.schemas.decoders.given
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.ObservingMode
import monocle.Focus
import org.typelevel.cats.time.*

import java.time.Instant
import scala.collection.immutable.SortedSet

// TODO Rename to Observation??
case class ObsSummary(
  id:                  Observation.Id,
  title:               String,
  subtitle:            Option[NonEmptyString],
  status:              ObsStatus,
  activeStatus:        ObsActiveStatus,
  scienceTargetIds:    AsterismIds,
  constraints:         ConstraintSet,
  timingWindows:       List[TimingWindow],
  attachmentIds:       SortedSet[ObsAttachment.Id],
  scienceRequirements: ScienceRequirements,
  observingMode:       Option[ObservingMode],
  visualizationTime:   Option[Instant],
  posAngleConstraint:  PosAngleConstraint,
  wavelength:          Option[Wavelength],
  groupId:             Option[Group.Id],
  groupIndex:          NonNegShort,
  validations:         List[ObservationValidation],
  observerNotes:       Option[NonEmptyString]
) derives Eq:
  lazy val configurationSummary: Option[String] = observingMode.map(_.toBasicConfiguration) match
    case Some(BasicConfiguration.GmosNorthLongSlit(grating, _, fpu, _)) =>
      s"GMOS-N ${grating.shortName} ${fpu.shortName}".some
    case Some(BasicConfiguration.GmosSouthLongSlit(grating, _, fpu, _)) =>
      s"GMOS-S ${grating.shortName} ${fpu.shortName}".some
    case _                                                              =>
      none

  val toModeOverride: Option[InstrumentOverrides] = observingMode.map {
    case n: ObservingMode.GmosNorthLongSlit =>
      val defaultMode   = GmosCcdMode(n.defaultXBin,
                                    n.defaultYBin,
                                    GmosAmpCount.Twelve,
                                    n.defaultAmpGain,
                                    n.defaultAmpReadMode
      )
      val overridenMode =
        List(n.explicitXBin, n.explicitYBin, n.explicitAmpGain, n.explicitAmpReadMode).foldLeft(
          defaultMode
        ) {
          case (mode, Some(x: GmosXBinning))    =>
            mode.copy(xBin = x)
          case (mode, Some(x: GmosYBinning))    =>
            mode.copy(yBin = x)
          case (mode, Some(x: GmosAmpGain))     =>
            mode.copy(ampGain = x)
          case (mode, Some(x: GmosAmpReadMode)) =>
            mode.copy(ampReadMode = x)
          case (mode, _)                        =>
            mode
        }

      GmosSpectroscopyOverrides(overridenMode.some, n.explicitRoi)
    case s: ObservingMode.GmosSouthLongSlit =>
      val defaultMode   = GmosCcdMode(s.defaultXBin,
                                    s.defaultYBin,
                                    GmosAmpCount.Twelve,
                                    s.defaultAmpGain,
                                    s.defaultAmpReadMode
      )
      val overridenMode =
        List(s.explicitXBin, s.explicitYBin, s.explicitAmpGain, s.explicitAmpReadMode).foldLeft(
          defaultMode
        ) {
          case (mode, Some(x: GmosXBinning))    =>
            mode.copy(xBin = x)
          case (mode, Some(x: GmosYBinning))    =>
            mode.copy(yBin = x)
          case (mode, Some(x: GmosAmpGain))     =>
            mode.copy(ampGain = x)
          case (mode, Some(x: GmosAmpReadMode)) =>
            mode.copy(ampReadMode = x)
          case (mode, _)                        =>
            mode
        }
      GmosSpectroscopyOverrides(overridenMode.some, s.explicitRoi)
  }

  lazy val constraintsSummary: String =
    s"${constraints.imageQuality.label} ${constraints.cloudExtinction.label} ${constraints.skyBackground.label} ${constraints.waterVapor.label}"

object ObsSummary:
  val id                  = Focus[ObsSummary](_.id)
  val title               = Focus[ObsSummary](_.title)
  val subtitle            = Focus[ObsSummary](_.subtitle)
  val status              = Focus[ObsSummary](_.status)
  val activeStatus        = Focus[ObsSummary](_.activeStatus)
  val scienceTargetIds    = Focus[ObsSummary](_.scienceTargetIds)
  val constraints         = Focus[ObsSummary](_.constraints)
  val timingWindows       = Focus[ObsSummary](_.timingWindows)
  val attachmentIds       = Focus[ObsSummary](_.attachmentIds)
  val scienceRequirements = Focus[ObsSummary](_.scienceRequirements)
  val observingMode       = Focus[ObsSummary](_.observingMode)
  val visualizationTime   = Focus[ObsSummary](_.visualizationTime)
  val posAngleConstraint  = Focus[ObsSummary](_.posAngleConstraint)
  val wavelength          = Focus[ObsSummary](_.wavelength)
  val groupId             = Focus[ObsSummary](_.groupId)
  val groupIndex          = Focus[ObsSummary](_.groupIndex)
  val validations         = Focus[ObsSummary](_.validations)
  val observerNotes       = Focus[ObsSummary](_.observerNotes)

  private case class TargetIdWrapper(id: Target.Id)
  private object TargetIdWrapper:
    given Decoder[TargetIdWrapper] = deriveDecoder

  private case class AttachmentIdWrapper(id: ObsAttachment.Id)
  private object AttachmentIdWrapper:
    given Decoder[AttachmentIdWrapper] = deriveDecoder

  given Decoder[ObsSummary] = Decoder.instance(c =>
    for {
      id                  <- c.get[Observation.Id]("id")
      title               <- c.get[String]("title")
      subtitle            <- c.get[Option[NonEmptyString]]("subtitle")
      status              <- c.get[ObsStatus]("status")
      activeStatus        <- c.get[ObsActiveStatus]("activeStatus")
      scienceTargetIds    <- c.downField("targetEnvironment").get[List[TargetIdWrapper]]("asterism")
      constraints         <- c.get[ConstraintSet]("constraintSet")
      timingWindows       <- c.get[List[TimingWindow]]("timingWindows")
      attachmentIds       <- c.get[List[AttachmentIdWrapper]]("obsAttachments")
      scienceRequirements <- c.get[ScienceRequirements]("scienceRequirements")
      observingMode       <- c.get[Option[ObservingMode]]("observingMode")
      visualizationTime   <- c.get[Option[Timestamp]]("visualizationTime")
      posAngleConstraint  <- c.get[PosAngleConstraint]("posAngleConstraint")
      wavelength          <- c.downField("scienceRequirements")
                               .downField("spectroscopy")
                               .get[Option[Wavelength]]("wavelength")
      groupId             <- c.get[Option[Group.Id]]("groupId")
      groupIndex          <- c.get[NonNegShort]("groupIndex")
      validations         <- c.get[List[ObservationValidation]]("validations")
      observerNotes       <- c.get[Option[NonEmptyString]]("observerNotes")
    } yield ObsSummary(
      id,
      title,
      subtitle,
      status,
      activeStatus,
      SortedSet.from(scienceTargetIds.map(_.id)),
      constraints,
      timingWindows,
      SortedSet.from(attachmentIds.map(_.id)),
      scienceRequirements,
      observingMode,
      visualizationTime.map(_.toInstant),
      posAngleConstraint,
      wavelength,
      groupId,
      groupIndex,
      validations,
      observerNotes
    )
  )
