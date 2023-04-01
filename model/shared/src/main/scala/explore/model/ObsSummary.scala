// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsStatus
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Target
import lucuma.core.util.TimeSpan
import lucuma.schemas.decoders.given
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.ConstraintsSummary
import monocle.Focus
import org.typelevel.cats.time.*
import io.circe.generic.semiauto.*
import lucuma.core.util.Timestamp
import io.circe.refined.given

import java.time.Instant
import io.circe.Decoder
import io.circe.JsonObject
import lucuma.core.model.ConstraintSet

case class ObsSummary(
  id:                 Observation.Id,
  title:              String,
  subtitle:           Option[NonEmptyString],
  status:             ObsStatus,
  activeStatus:       ObsActiveStatus,
  executionTime:      TimeSpan,
  scienceTargetIds:   Set[Target.Id],
  constraints:        ConstraintSet,
  configuration:      Option[BasicConfiguration],
  visualizationTime:  Option[Instant],
  posAngleConstraint: Option[PosAngleConstraint],
  wavelength:         Option[Wavelength]
) derives Eq:
  lazy val configurationSummary: Option[String] = configuration match
    case Some(BasicConfiguration.GmosNorthLongSlit(grating, _, fpu, _)) =>
      s"GMOS-N ${grating.shortName} ${fpu.shortName}".some
    case Some(BasicConfiguration.GmosSouthLongSlit(grating, _, fpu, _)) =>
      s"GMOS-S ${grating.shortName} ${fpu.shortName}".some
    case _                                                              =>
      none

  lazy val constraintsSummary: String =
    s"${constraints.imageQuality.label} ${constraints.cloudExtinction.label} ${constraints.skyBackground.label} ${constraints.waterVapor.label}"

object ObsSummary:
  val id                 = Focus[ObsSummary](_.id)
  val title              = Focus[ObsSummary](_.title)
  val subtitle           = Focus[ObsSummary](_.subtitle)
  val status             = Focus[ObsSummary](_.status)
  val activeStatus       = Focus[ObsSummary](_.activeStatus)
  val scienceTargetIds   = Focus[ObsSummary](_.scienceTargetIds)
  val constraints        = Focus[ObsSummary](_.constraints)
  val configuration      = Focus[ObsSummary](_.configuration)
  val visualizationTime  = Focus[ObsSummary](_.visualizationTime)
  val posAngleConstraint = Focus[ObsSummary](_.posAngleConstraint)
  val wavelength         = Focus[ObsSummary](_.wavelength)

  private case class TargetIdWrapper(id: Target.Id)
  private object TargetIdWrapper:
    given Decoder[TargetIdWrapper] = deriveDecoder

  given Decoder[ObsSummary] = Decoder.instance(c =>
    for {
      id                 <- c.get[Observation.Id]("id")
      title              <- c.get[String]("title")
      subtitle           <- c.get[Option[NonEmptyString]]("subtitle")
      constraints        <- c.get[ConstraintSet]("constraintSet")
      status             <- c.get[ObsStatus]("status")
      activeStatus       <- c.get[ObsActiveStatus]("activeStatus")
      executionTime      <- c.downField("plannedTime").get[TimeSpan]("execution")
      scienceTargetIds   <- c.downField("targetEnvironment").get[List[TargetIdWrapper]]("asterism")
      observingMode      <- c.get[Option[BasicConfiguration]]("observingMode")
      visualizationTime  <- c.get[Option[Timestamp]]("visualizationTime")
      posAngleConstraint <- c.get[Option[PosAngleConstraint]]("posAngleConstraint")
      wavelength         <- c.downField("scienceRequirements")
                              .downField("spectroscopy")
                              .get[Option[Wavelength]]("wavelength")
    } yield ObsSummary(
      id,
      title,
      subtitle,
      status,
      activeStatus,
      executionTime,
      scienceTargetIds.map(_.id).toSet,
      constraints,
      observingMode,
      visualizationTime.map(_.toInstant),
      posAngleConstraint,
      wavelength
    )
  )
