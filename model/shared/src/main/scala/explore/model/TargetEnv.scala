package explore.model

import explore.model.decoders._
import io.circe.Decoder
import io.circe.generic.semiauto._
import lucuma.core.model.Target
import lucuma.core.model.TargetEnvironment

case class TargetEnv(id: TargetEnvironment.Id, scienceTargets: List[Target])

object TargetEnv {
  implicit val decoder: Decoder[TargetEnv] = deriveDecoder
}
