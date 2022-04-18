// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.syntax.all._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enum.ObsActiveStatus
import lucuma.core.enum.ObsStatus
import lucuma.core.model.Observation
import lucuma.core.model.Target
import monocle.Focus
import org.typelevel.cats.time._

import java.time.Duration

trait ObsSummary {
  val id: Observation.Id
  val status: ObsStatus
  val activeStatus: ObsActiveStatus
  val duration: Duration
}

object ObsSummary {
  implicit val eqObsSummary: Eq[ObsSummary] = Eq.instance((_: ObsSummary, _: ObsSummary) match {
    case (a: ObsSummaryWithConstraints, b: ObsSummaryWithConstraints)                 =>
      a === b
    case (a: ObsSummaryWithTitleAndConstraints, b: ObsSummaryWithTitleAndConstraints) =>
      a === b
    case _                                                                            =>
      false
  })
}

trait ObsWithConstraints extends ObsSummary {
  val constraints: ConstraintsSummary

  lazy val constraintsSummary = constraints.summaryString
}

trait ObsWithConf extends ObsSummary {
  def scienceConfiguration: Option[ScienceConfiguration]
  val conf: String = scienceConfiguration match {
    case Some(GmosNorthLongSlit(_, d, _, _)) => s"GMOS-N ${d.shortName}"
    case Some(GmosSouthLongSlit(_, d, _, _)) => s"GMOS-S ${d.shortName}"
    case _                                   => s"-"
  }
}

trait ObsWithTitle extends ObsSummary {
  val title: NonEmptyString
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

object ObsSummaryWithConstraints {
  val id = Focus[ObsSummaryWithConstraints](_.id)

  implicit val eqObsSummaryWithConstraints: Eq[ObsSummaryWithConstraints] =
    Eq.by(o => (o.id, o.constraints, o.status, o.activeStatus, o.duration, o.scienceTargetIds))
}

case class ObsSummaryWithTitleAndConstraints(
  override val id:           Observation.Id,
  override val title:        NonEmptyString,
  override val subtitle:     Option[NonEmptyString],
  override val constraints:  ConstraintsSummary,
  override val status:       ObsStatus,
  override val activeStatus: ObsActiveStatus,
  override val duration:     Duration
) extends ObsSummary
    with ObsWithTitle
    with ObsWithConstraints

object ObsSummaryWithTitleAndConstraints {
  val id           = Focus[ObsSummaryWithTitleAndConstraints](_.id)
  val subtitle     = Focus[ObsSummaryWithTitleAndConstraints](_.subtitle)
  val status       = Focus[ObsSummaryWithTitleAndConstraints](_.status)
  val activeStatus = Focus[ObsSummaryWithTitleAndConstraints](_.activeStatus)

  implicit val eqObsSummaryWithTitleAndConstraints: Eq[ObsSummaryWithTitleAndConstraints] =
    Eq.by(o => (o.id, o.title, o.subtitle, o.constraints, o.status, o.activeStatus, o.duration))
}

case class ObsSummaryWithTitleAndConf(
  override val id:                   Observation.Id,
  override val title:                NonEmptyString,
  override val subtitle:             Option[NonEmptyString],
  override val status:               ObsStatus,
  override val activeStatus:         ObsActiveStatus,
  override val duration:             Duration,
  override val scienceConfiguration: Option[ScienceConfiguration]
) extends ObsSummary
    with ObsWithTitle
    with ObsWithConf

object ObsSummaryWithTitleAndConf {
  val id = Focus[ObsSummaryWithTitleAndConf](_.id)

  implicit val eqObsSummaryWithTargetsAndConf: Eq[ObsSummaryWithTitleAndConf] =
    Eq.by(o => (o.id, o.title, o.subtitle, o.status, o.activeStatus, o.duration, o.conf))
}
