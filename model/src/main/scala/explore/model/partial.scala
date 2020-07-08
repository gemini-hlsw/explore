package explore.model

import explore.model.enum.ObsStatus
import java.time.Duration
import io.circe.Decoder
import io.circe.HCursor
import explore.model.decoders._
import monocle.macros.Lenses

@Lenses
final case class TargetSummary(id: SiderealTarget.Id, name: String)

object TargetSummary {
  def fromTarget(target: SiderealTarget): TargetSummary =
    TargetSummary(target.id, target.name) // This is an opportunity to use chimney

  implicit val decoder = new Decoder[TargetSummary] {
    final def apply(c: HCursor): Decoder.Result[TargetSummary] =
      for {
        id   <- c.downField("id").as[SiderealTarget.Id]
        name <- c.downField("name").as[String]
      } yield TargetSummary(id, name)
  }
}

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

@Lenses
final case class ObsSummary(
  id:          ExploreObservation.Id,
  target:      TargetSummary,
  status:      ObsStatus,
  conf:        String,
  constraints: ConstraintsSummary,
  duration:    Duration
)

object ObsSummary {
  def fromObs(obs: ExploreObservation): ObsSummary =
    ObsSummary(obs.id,
               TargetSummary.fromTarget(obs.target),
               obs.status,
               obs.conf,
               ConstraintsSummary.fromConstraints(obs.constraints),
               obs.duration
    )

  def decoderForTarget(target: SiderealTarget) =
    new Decoder[ObsSummary] {
      final def apply(c: HCursor): Decoder.Result[ObsSummary] =
        for {
          id          <- c.downField("id").as[ExploreObservation.Id]
          status      <- c.downField("status").as[ObsStatus]
          conf        <- c.downField("configuration").as[String]
          constraints <- c.downField("constraints").as[ConstraintsSummary]
          duration    <- c.downField("duration_seconds").as[Long].map(Duration.ofSeconds)
        } yield ObsSummary(
          id,
          TargetSummary.fromTarget(target),
          status,
          conf,
          constraints,
          duration
        )
    }
}
