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
import monocle.Focus
import org.typelevel.cats.time.*

import java.time.Duration
import java.time.Instant

trait ObsSummary {
  val id: Observation.Id
  val status: ObsStatus
  val activeStatus: ObsActiveStatus
  val duration: Duration
}

object ObsSummary {
  implicit val eqObsSummary: Eq[ObsSummary] = Eq.instance((_: ObsSummary, _: ObsSummary) match {
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

trait ObsWithConstraints extends ObsSummary {
  val constraints: ConstraintsSummary

  lazy val constraintsSummary = constraints.summaryString
}

trait ObsWithConf extends ObsSummary {
  def configuration: Option[BasicConfiguration]

  val conf: String = configuration match {
    case Some(n: BasicConfiguration.GmosNorthLongSlit) => s"GMOS-N ${n.grating.shortName}"
    case Some(s: BasicConfiguration.GmosSouthLongSlit) => s"GMOS-S ${s.grating.shortName}"
    case _                                             => s"-"
  }
}

trait ObsWithVizTime extends ObsSummary {
  def visualizationTime: Option[Instant]
}

trait ObsWithTitle extends ObsSummary {
  val title: String
  val subtitle: Option[NonEmptyString]
}

case class ObsSummaryWithConstraints(
  override val id:           Observation.Id,
  override val constraints:  ConstraintsSummary,
  override val status:       ObsStatus,
  override val activeStatus: ObsActiveStatus,
  override val duration:     Duration,
  scienceTargetIds:          Set[Target.Id]
) extends ObsSummary
    with ObsWithConstraints
    derives Eq

object ObsSummaryWithConstraints {
  val id = Focus[ObsSummaryWithConstraints](_.id)
}

case class ObsSummaryWithTitleAndConstraints(
  override val id:           Observation.Id,
  override val title:        String,
  override val subtitle:     Option[NonEmptyString],
  override val constraints:  ConstraintsSummary,
  override val status:       ObsStatus,
  override val activeStatus: ObsActiveStatus,
  override val duration:     Duration
) extends ObsSummary
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
      duration,
      none,
      none
    )

  def toConstraintsAndConf(targetIds: Set[Target.Id]) =
    ObsSummaryWithConstraintsAndConf(
      id,
      constraints,
      status,
      activeStatus,
      duration,
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
  override val duration:          Duration,
  override val configuration:     Option[BasicConfiguration],
  override val visualizationTime: Option[Instant]
) extends ObsSummary
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
  override val duration:      Duration,
  override val configuration: Option[BasicConfiguration]
) extends ObsSummary
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
  override val duration:          Duration,
  scienceTargetIds:               Set[Target.Id],
  override val configuration:     Option[BasicConfiguration],
  override val visualizationTime: Option[Instant],
  posAngleConstraint:             Option[PosAngleConstraint],
  wavelength:                     Option[Wavelength]
) extends ObsSummary
    with ObsWithConstraints
    with ObsWithVizTime
    with ObsWithConf
    derives Eq

object ObsSummaryWithConstraintsAndConf {
  val id                = Focus[ObsSummaryWithConstraintsAndConf](_.id)
  val configuration     = Focus[ObsSummaryWithConstraintsAndConf](_.configuration)
  val visualizationTime = Focus[ObsSummaryWithConstraintsAndConf](_.visualizationTime)
}
