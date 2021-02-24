// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import explore.model.enum.ObsStatus
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.JsonObject
import io.circe.syntax._
import lucuma.core.model.Asterism
import lucuma.core.model.Observation
import lucuma.core.model.Target
import monocle.macros.Lenses

import java.time.Duration
import java.time.temporal.ChronoUnit

@Lenses
final case class ObsSummary(
  id:                Observation.Id,
  name:              Option[String],
  status:            ObsStatus = ObsStatus.New,
  conf:              String = "GMOS-N R831 1x300",
  constraints:       String = "<0.7\" <0.3 mag Bright",
  duration:          Duration = Duration.of(93, ChronoUnit.MINUTES),
  observationTarget: Option[Either[Asterism.Id, Target.Id]]
)

object ObsSummary {
  implicit val eqObsSummary: Eq[ObsSummary] = Eq.by(x => (x.id, x.name, x.observationTarget))

  implicit val obsSummaryEncoder: Encoder[ObsSummary] = new Encoder[ObsSummary] {
    final def apply(c: ObsSummary): Json = {
      val common = JsonObject(("id", c.id.asJson), ("name", c.name.asJson))
      (c.observationTarget match {
        case None    => common
        case Some(t) =>
          common.add("observationTarget",
                     t match {
                       case Right(tid) => Json.obj(("target_id", tid.asJson))
                       case Left(aid)  => Json.obj(("asterism_id", aid.asJson))
                     }
          )
      }).asJson
    }

  }

  implicit val obsTargetDecoder: Decoder[Either[Asterism.Id, Target.Id]] =
    new Decoder[Either[Asterism.Id, Target.Id]] {
      final def apply(c: HCursor): Decoder.Result[Either[Asterism.Id, Target.Id]] =
        (for {
          aid <- c.downField("asterism_id").as[Option[Asterism.Id]]
          tid <- c.downField("target_id").as[Option[Target.Id]]
        } yield (aid, tid) match {
          case (Some(aid), None) => Right(Left(aid))
          case (None, Some(tid)) => Right(Right(tid))
          case _                 => Left(DecodingFailure("graphql schema doesn't allow this", Nil))
        }).flatten
    }

  implicit val obsSummaryDecoder: Decoder[ObsSummary] = new Decoder[ObsSummary] {
    final def apply(c: HCursor): Decoder.Result[ObsSummary] =
      for {
        id   <- c.downField("id").as[Observation.Id]
        name <- c.downField("name").as[Option[String]]
        ot   <- c.downField("observationTarget").as[Option[Either[Asterism.Id, Target.Id]]]
      } yield ObsSummary(id = id, name = name, observationTarget = ot)
  }
}
