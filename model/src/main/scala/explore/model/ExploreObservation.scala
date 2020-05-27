package explore.model

import cats.Eq
import cats.implicits._
import explore.model.enum.ObsStatus
import java.util.UUID
import java.time.Duration
import monocle.macros.Lenses

@Lenses
final case class ExploreObservation(
  id:          UUID,
  target:      SiderealTarget,
  status:      ObsStatus,
  conf:        String,
  constraints: String,
  duration:    Duration
)

object ExploreObservation {
  implicit val equalObservation: Eq[ExploreObservation] =
    Eq.by(_.id)
}
