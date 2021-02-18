// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import io.circe.Decoder
import io.circe.HCursor
import monocle.macros.Lenses

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
