// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all._
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder

// We can't use Enumerated since it encodes/decodes as SCREAMING_SNAKE.
sealed abstract class AimType(val typeName: String) { self =>
  // Using self.type allows us to use eg: Target.asJosn without need of widening.
  implicit val aimTypeEncoder: Encoder[self.type] =
    Encoder.encodeString.contramap[self.type](_.typeName)
}

object AimType {
  case object Target   extends AimType("Target")
  case object Asterism extends AimType("Asterism")

  implicit val aimTypeDecoder: Decoder[AimType] =
    Decoder.instance[AimType] { c =>
      c.as[String]
        .flatMap(_ match {
          case Target.typeName   => Target.asRight
          case Asterism.typeName => Asterism.asRight
          case _                 => DecodingFailure("Unsupported aim type", c.history).asLeft
        })
    }
}
