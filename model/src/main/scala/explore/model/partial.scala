// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all._
import explore.model.enum.ObsStatus
import io.circe.{ Decoder, HCursor }
import lucuma.core.model.Observation
import monocle.macros.Lenses

import java.time.Duration
import java.time.temporal.ChronoUnit

@Lenses
final case class ConstraintsSummary(id: Constraints.Id, name: String)

object ConstraintsSummary {
  def fromConstraints(constraints: Constraints): ConstraintsSummary =
    ConstraintsSummary(constraints.id, constraints.name)

  implicit val decoder = new Decoder[ConstraintsSummary] {
    final def apply(c: HCursor): Decoder.Result[ConstraintsSummary] =
      for {
        id   <- c.downField("id").as[Constraints.Id]
        name <- c.downField("name").as[String]
      } yield ConstraintsSummary(id, name)
  }
}

trait ObsSummary {
  val id: Observation.Id
  val name: Option[String]
  val status: ObsStatus   = ObsStatus.New
  val conf: String        = "GMOS-N R831 1x300"
  val constraints: String = "<0.7\" <0.3 mag Bright"
  val duration: Duration  = Duration.of(93, ChronoUnit.MINUTES)
}

object ObsSummary {
  def apply(_id: Observation.Id, _name: String): ObsSummary =
    new ObsSummary {
      val id   = _id
      val name = _name.some
    }
}
