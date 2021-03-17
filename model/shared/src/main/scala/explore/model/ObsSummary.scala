// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import cats.syntax.all._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.enum.ObsStatus
import io.circe.Decoder
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.JsonObject
import io.circe.refined._
import io.circe.syntax._
import lucuma.core.model.Asterism
import lucuma.core.model.Observation
import lucuma.core.model.Target
import monocle.macros.Lenses

import java.time.Duration
import java.time.temporal.ChronoUnit

@Lenses
final case class ObsSummary(
  id:          Observation.Id,
  name:        Option[NonEmptyString],
  status:      ObsStatus = ObsStatus.New,
  constraints: Option[ConstraintsSummary],
  conf:        String = "GMOS-N R831 1x300",
  duration:    Duration = Duration.of(93, ChronoUnit.MINUTES),
  pointingId:  Option[PointingId]
) {
  def constraintsSummary = constraints.map(_.summaryString).getOrElse("No constraints")
}

object ObsSummary {

  implicit val eqObsSummary: Eq[ObsSummary] =
    Eq.by(x => (x.id, x.name, x.pointingId, x.constraints))

  implicit val obsSummaryEncoder: Encoder[ObsSummary] = new Encoder[ObsSummary] {
    final def apply(c: ObsSummary): Json = {
      val common = JsonObject(("id", c.id.asJson), ("name", c.name.asJson))

      val withConstraints = c.constraints match {
        case Some(cs) => common.add("constraintSet", cs.asJson)
        case None     => common
      }

      (c.pointingId match {
        case None    => withConstraints
        case Some(t) =>
          withConstraints.add(
            "observationTarget",
            t match {
              case Right(tid) =>
                Json.obj("type" -> PointingType.Target.asJson, "target_id" -> tid.asJson)
              case Left(aid)  =>
                Json.obj("type" -> PointingType.Asterism.asJson, "asterism_id" -> aid.asJson)
            }
          )
      }).asJson
    }

  }

  implicit val obsTargetDecoder: Decoder[PointingId] =
    new Decoder[PointingId] {
      final def apply(c: HCursor): Decoder.Result[PointingId] =
        c.downField("type").as[PointingType].flatMap {
          case PointingType.Target   => c.downField("target_id").as[Target.Id].map(_.asRight)
          case PointingType.Asterism => c.downField("asterism_id").as[Asterism.Id].map(_.asLeft)
        }
    }

  implicit val obsSummaryDecoder: Decoder[ObsSummary] = new Decoder[ObsSummary] {
    final def apply(c: HCursor): Decoder.Result[ObsSummary] =
      for {
        id         <- c.downField("id").as[Observation.Id]
        name       <- c.downField("name").as[Option[NonEmptyString]]
        pointingId <- c.downField("observationTarget").as[Option[PointingId]]
        cs         <- c.downField("constraintSet").as[Option[ConstraintsSummary]]
      } yield ObsSummary(id, name, pointingId = pointingId, constraints = cs)
  }
}
