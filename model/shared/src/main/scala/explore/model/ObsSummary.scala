// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.syntax.all._
import eu.timepit.refined.auto._
import io.chrisdavenport.cats.time._
import lucuma.core.enum.ObsActiveStatus
import lucuma.core.enum.ObsStatus
import lucuma.core.model.Observation
import lucuma.core.model.TargetEnvironment
import monocle.Focus

import java.time.Duration

trait ObsSummary {
  val id: Observation.Id
  val status: ObsStatus
  val activeStatus: ObsActiveStatus
  val duration: Duration
}

object ObsSummary {
  implicit val eqObsSummary: Eq[ObsSummary] = Eq.instance((_: ObsSummary, _: ObsSummary) match {
    case (a: ObsSummaryWithConstraints, b: ObsSummaryWithConstraints)                     =>
      a === b
    case (a: ObsSummaryWithTargetsAndConstraints, b: ObsSummaryWithTargetsAndConstraints) =>
      a === b
    case _                                                                                =>
      false
  })
}

trait ObsWithConstraints extends ObsSummary {
  val constraints: ConstraintsSummary
  // val targetEnvId: TargetEnvironment.Id

  lazy val constraintsSummary = constraints.summaryString
}

trait ObsWithConf extends ObsSummary {
  val conf: String = "GMOS-N R831 1x300"
}

trait ObsWithTargets extends ObsSummary {
  val targets: List[TargetSummary]

  lazy val targetNames: String =
    targets.map(_.name).mkString(";")
}

case class ObsSummaryWithConstraints(
  override val id:           Observation.Id,
  override val constraints:  ConstraintsSummary,
  override val status:       ObsStatus,
  override val activeStatus: ObsActiveStatus,
  override val duration:     Duration,
  targetEnvId:               TargetEnvironment.Id
) extends ObsSummary
    with ObsWithConstraints

object ObsSummaryWithConstraints {
  val id          = Focus[ObsSummaryWithConstraints](_.id)
  val targetEnvId = Focus[ObsSummaryWithConstraints](_.targetEnvId)

  implicit val eqObsSummaryWithConstraints: Eq[ObsSummaryWithConstraints] =
    Eq.by(o => (o.id, o.constraints, o.status, o.activeStatus, o.duration, o.targetEnvId))
}

case class ObsSummaryWithTargetsAndConstraints(
  override val id:           Observation.Id,
  override val targets:      List[TargetSummary],
  override val constraints:  ConstraintsSummary,
  override val status:       ObsStatus,
  override val activeStatus: ObsActiveStatus,
  override val duration:     Duration
) extends ObsSummary
    with ObsWithTargets
    with ObsWithConstraints

object ObsSummaryWithTargetsAndConstraints {
  val id           = Focus[ObsSummaryWithTargetsAndConstraints](_.id)
  val status       = Focus[ObsSummaryWithTargetsAndConstraints](_.status)
  val activeStatus = Focus[ObsSummaryWithTargetsAndConstraints](_.activeStatus)

  implicit val eqObsSummaryWithTargetsAndConstraints: Eq[ObsSummaryWithTargetsAndConstraints] =
    Eq.by(o => (o.id, o.targets, o.constraints, o.status, o.activeStatus, o.duration))
}

case class ObsSummaryWithTargetsAndConf(
  override val id:           Observation.Id,
  override val targets:      List[TargetSummary],
  override val status:       ObsStatus,
  override val activeStatus: ObsActiveStatus,
  override val duration:     Duration
) extends ObsSummary
    with ObsWithTargets
    with ObsWithConf

object ObsSummaryWithTargetsAndConf {
  val id = Focus[ObsSummaryWithTargetsAndConf](_.id)

  implicit val eqObsSummaryWithTargetsAndConf: Eq[ObsSummaryWithTargetsAndConf] =
    Eq.by(o => (o.id, o.targets, o.status, o.activeStatus, o.duration, o.conf))
}
