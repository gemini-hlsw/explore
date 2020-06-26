// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.implicits._
import gem.util.Enumerated
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.HCursor
import io.circe.JsonObject
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder
import gsp.math.RightAscension
import gsp.math.Declination
import gsp.math.Coordinates
import gsp.math.Epoch
import gsp.math.ProperMotion
import explore.model.enum.ObsStatus
import java.time.Duration

object decoders {

  implicit def enumDecoder[E: Enumerated]: Decoder[E] =
    new Decoder[E] {
      final def apply(c: HCursor): Decoder.Result[E] =
        // TODO Obtain the failure CursorOp list from c.
        c.as[String]
          .flatMap(s =>
            Enumerated[E]
              .fromTag(s)
              .toRight(DecodingFailure(s"Invalid Enumerated value [$s] on [$c].", List.empty))
          )
    }

  implicit val raDecoder = new Decoder[RightAscension] {
    final def apply(c: HCursor): Decoder.Result[RightAscension] =
      for {
        raStr <- c.as[String]
        ra    <- RightAscension.fromStringHMS
                .getOption(raStr)
                .toRight(DecodingFailure(s"Invalid RightAscension [$raStr]", List.empty))
      } yield ra
  }

  implicit val decDecoder = new Decoder[Declination] {
    final def apply(c: HCursor): Decoder.Result[Declination] =
      for {
        decStr <- c.as[String]
        dec    <- Declination.fromStringSignedDMS
                 .getOption(decStr)
                 .toRight(DecodingFailure(s"Invalid Declination [$decStr]", List.empty))
      } yield dec
  }

  implicit val siderealTargetDecoder = new Decoder[SiderealTarget] {
    final def apply(c: HCursor): Decoder.Result[SiderealTarget] =
      for {
        id    <- c.downField("id").as[SiderealTarget.Id]
        name  <- c.downField("name").as[String]
        ra    <- c.downField("ra").as[RightAscension]
        dec   <- c.downField("dec").as[Declination]
        coords = ProperMotion(Coordinates(ra, dec), Epoch.J2000, none, none, none)
      } yield SiderealTarget(id, name, coords)
  }
}
