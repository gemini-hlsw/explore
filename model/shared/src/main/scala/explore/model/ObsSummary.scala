// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.syntax.all._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import io.chrisdavenport.cats.time._
import lucuma.core.enum.ObsActiveStatus
import lucuma.core.enum.ObsStatus
import lucuma.core.model.Observation
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
    case (a: ObsSummaryWithConstraints, b: ObsSummaryWithConstraints) =>
      a === b
    case (a: ObsSummaryWithPointingAndConstraints, b: ObsSummaryWithPointingAndConstraints) =>
      a === b
    case _ =>
      false
  })
}

trait ObsWithConstraints extends ObsSummary {
  val constraints: ConstraintsSummary

  lazy val constraintsSummary = constraints.summaryString
}

trait ObsWithConf extends ObsSummary {
  val conf: String = "GMOS-N R831 1x300"
}

trait ObsWithPointing extends ObsSummary {
  val pointing: Option[Pointing]

  lazy val pointingName: NonEmptyString =
    pointing match {
      case None                                   => "<No Target>"
      case Some(Pointing.PointingTarget(_, name)) => name
      case Some(Pointing.PointingAsterism(_, name, targets)) =>
        name match {
          case Some(aname) => aname
          case None        => NonEmptyString.unsafeFrom(targets.map(_.name).mkString("-"))
        }
    }
}

case class ObsSummaryWithConstraints(
  override val id:           Observation.Id,
  override val constraints:  ConstraintsSummary,
  override val status:       ObsStatus,
  override val activeStatus: ObsActiveStatus,
  override val duration:     Duration
) extends ObsSummary
    with ObsWithConstraints

object ObsSummaryWithConstraints {
  implicit val eqObsSummaryWithConstraints: Eq[ObsSummaryWithConstraints] =
    Eq.by(o => (o.id, o.constraints, o.status, o.activeStatus, o.duration))
}

case class ObsSummaryWithPointingAndConstraints(
  override val id:           Observation.Id,
  override val pointing:     Option[Pointing],
  override val constraints:  ConstraintsSummary,
  override val status:       ObsStatus,
  override val activeStatus: ObsActiveStatus,
  override val duration:     Duration
) extends ObsSummary
    with ObsWithPointing
    with ObsWithConstraints

object ObsSummaryWithPointingAndConstraints {
  val id           = Focus[ObsSummaryWithPointingAndConstraints](_.id)
  val status       = Focus[ObsSummaryWithPointingAndConstraints](_.status)
  val activeStatus = Focus[ObsSummaryWithPointingAndConstraints](_.activeStatus)

  implicit val eqObsSummaryWithPointingAndConstraints: Eq[ObsSummaryWithPointingAndConstraints] =
    Eq.by(o => (o.id, o.pointing, o.constraints, o.status, o.activeStatus, o.duration))
}

case class ObsSummaryWithPointingAndConf(
  override val id:           Observation.Id,
  override val pointing:     Option[Pointing],
  override val status:       ObsStatus,
  override val activeStatus: ObsActiveStatus,
  override val duration:     Duration
) extends ObsSummary
    with ObsWithPointing
    with ObsWithConf

object ObsSummaryWithPointingAndConf {
  val id = Focus[ObsSummaryWithPointingAndConf](_.id)

  implicit val eqObsSummaryWithPointingAndConf: Eq[ObsSummaryWithPointingAndConf] =
    Eq.by(o => (o.id, o.pointing, o.status, o.activeStatus, o.duration, o.conf))
}
