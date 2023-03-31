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

trait ObsSummaryOld {
  val id: Observation.Id
  val status: ObsStatus
  val activeStatus: ObsActiveStatus
  val executionTime: TimeSpan
}

object ObsSummaryOld {
  implicit val eqObsSummary: Eq[ObsSummaryOld] =
    Eq.instance((_: ObsSummaryOld, _: ObsSummaryOld) match {
      case (a: ObsSummaryWithConstraints, b: ObsSummaryWithConstraints)                         =>
        a === b
      case (a: ObsSummaryWithConstraintsAndConf, b: ObsSummaryWithConstraintsAndConf)           =>
        a === b
      case (a: ObsSummaryWithTitleAndConstraints, b: ObsSummaryWithTitleAndConstraints)         =>
        a === b
      case (a: ObsSummaryWithTitleConstraintsAndConf, b: ObsSummaryWithTitleConstraintsAndConf) =>
        a === b
      case _                                                                                    =>
        false
    })
}

trait ObsWithConstraints extends ObsSummaryOld {
  val constraints: ConstraintsSummary

  lazy val constraintsSummary = constraints.summaryString
}

trait ObsWithConf extends ObsSummaryOld {
  def configuration: Option[BasicConfiguration]

  val conf: String = configuration match {
    case Some(n: BasicConfiguration.GmosNorthLongSlit) =>
      s"GMOS-N ${n.grating.shortName} ${n.fpu.shortName}"
    case Some(s: BasicConfiguration.GmosSouthLongSlit) =>
      s"GMOS-S ${s.grating.shortName} ${s.fpu.shortName}"
    case _                                             =>
      s"-"
  }
}

trait ObsWithVizTime extends ObsSummaryOld {
  def visualizationTime: Option[Instant]
}

trait ObsWithTitle extends ObsSummaryOld {
  val title: String
  val subtitle: Option[NonEmptyString]
}

case class ObsSummaryWithConstraints(
  override val id:            Observation.Id,
  override val constraints:   ConstraintsSummary,
  override val status:        ObsStatus,
  override val activeStatus:  ObsActiveStatus,
  override val executionTime: TimeSpan,
  scienceTargetIds:           Set[Target.Id]
) extends ObsSummaryOld
    with ObsWithConstraints
    derives Eq

object ObsSummaryWithConstraints {
  val id = Focus[ObsSummaryWithConstraints](_.id)
}

case class ObsSummaryWithTitleAndConstraints(
  override val id:            Observation.Id,
  override val title:         String,
  override val subtitle:      Option[NonEmptyString],
  override val constraints:   ConstraintsSummary,
  override val status:        ObsStatus,
  override val activeStatus:  ObsActiveStatus,
  override val executionTime: TimeSpan
) extends ObsSummaryOld
    with ObsWithTitle
    with ObsWithConstraints
    derives Eq {
  def toTitleAndConstraints =
    ObsSummaryWithTitleConstraintsAndConf(
      id,
      title,
      subtitle,
      constraints,
      status,
      activeStatus,
      executionTime,
      none,
      none
    )

  def toConstraintsAndConf(targetIds: Set[Target.Id]) =
    ObsSummaryWithConstraintsAndConf(
      id,
      constraints,
      status,
      activeStatus,
      executionTime,
      targetIds,
      none,
      none,
      none,
      none
    )
}

object ObsSummaryWithTitleAndConstraints {
  val id           = Focus[ObsSummaryWithTitleAndConstraints](_.id)
  val title        = Focus[ObsSummaryWithTitleAndConstraints](_.title)
  val subtitle     = Focus[ObsSummaryWithTitleAndConstraints](_.subtitle)
  val status       = Focus[ObsSummaryWithTitleAndConstraints](_.status)
  val activeStatus = Focus[ObsSummaryWithTitleAndConstraints](_.activeStatus)
}

case class ObsSummaryWithTitleConstraintsAndConf(
  override val id:                Observation.Id,
  override val title:             String,
  override val subtitle:          Option[NonEmptyString],
  override val constraints:       ConstraintsSummary,
  override val status:            ObsStatus,
  override val activeStatus:      ObsActiveStatus,
  override val executionTime:     TimeSpan,
  override val configuration:     Option[BasicConfiguration],
  override val visualizationTime: Option[Instant]
) extends ObsSummaryOld
    with ObsWithTitle
    with ObsWithConstraints
    with ObsWithVizTime
    with ObsWithConf
    derives Eq

object ObsSummaryWithTitleConstraintsAndConf {
  val id           = Focus[ObsSummaryWithTitleConstraintsAndConf](_.id)
  val subtitle     = Focus[ObsSummaryWithTitleConstraintsAndConf](_.subtitle)
  val status       = Focus[ObsSummaryWithTitleConstraintsAndConf](_.status)
  val activeStatus = Focus[ObsSummaryWithTitleConstraintsAndConf](_.activeStatus)
}

case class ObsSummaryWithTitleAndConf(
  override val id:            Observation.Id,
  override val title:         String,
  override val subtitle:      Option[NonEmptyString],
  override val status:        ObsStatus,
  override val activeStatus:  ObsActiveStatus,
  override val executionTime: TimeSpan,
  override val configuration: Option[BasicConfiguration]
) extends ObsSummaryOld
    with ObsWithTitle
    with ObsWithConf
    derives Eq

object ObsSummaryWithTitleAndConf {
  val id = Focus[ObsSummaryWithTitleAndConf](_.id)
}

case class ObsSummaryWithConstraintsAndConf(
  override val id:                Observation.Id,
  override val constraints:       ConstraintsSummary,
  override val status:            ObsStatus,
  override val activeStatus:      ObsActiveStatus,
  override val executionTime:     TimeSpan,
  scienceTargetIds:               Set[Target.Id],
  override val configuration:     Option[BasicConfiguration],
  override val visualizationTime: Option[Instant],
  posAngleConstraint:             Option[PosAngleConstraint],
  wavelength:                     Option[Wavelength]
) extends ObsSummaryOld
    with ObsWithConstraints
    with ObsWithVizTime
    with ObsWithConf
    derives Eq

object ObsSummaryWithConstraintsAndConf:
  val id                = Focus[ObsSummaryWithConstraintsAndConf](_.id)
  val configuration     = Focus[ObsSummaryWithConstraintsAndConf](_.configuration)
  val visualizationTime = Focus[ObsSummaryWithConstraintsAndConf](_.visualizationTime)

  private case class TargetIdWrapper(id: Target.Id)
  private object TargetIdWrapper:
    given Decoder[TargetIdWrapper] = deriveDecoder

  given Decoder[ObsSummaryWithConstraintsAndConf] = Decoder.instance(c =>
    for {
      id                 <- c.get[Observation.Id]("id")
      constraints        <- c.get[ConstraintsSummary]("constraintSet")
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
    } yield ObsSummaryWithConstraintsAndConf(
      id,
      constraints,
      status,
      activeStatus,
      executionTime,
      scienceTargetIds.map(_.id).toSet,
      observingMode,
      visualizationTime.map(_.toInstant),
      posAngleConstraint,
      wavelength
    )
  )

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
  val conf: String = configuration match
    case Some(n: BasicConfiguration.GmosNorthLongSlit) =>
      s"GMOS-N ${n.grating.shortName} ${n.fpu.shortName}"
    case Some(s: BasicConfiguration.GmosSouthLongSlit) =>
      s"GMOS-S ${s.grating.shortName} ${s.fpu.shortName}"
    case _                                             =>
      s"-"

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
