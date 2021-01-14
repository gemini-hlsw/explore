// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import explore.model.enum._
import io.circe.refined._
import io.circe.syntax._
import io.circe.{ Encoder, Json }
import lucuma.core.math.{ Declination, RightAscension }
import sttp.model.Uri

import ModelOptics._

object encoders {
  implicit val uriEncoder: Encoder[Uri] = Encoder.encodeString.contramap[Uri](_.toString)

  implicit val raEncoder: Encoder[RightAscension] =
    Encoder.encodeString
      .contramap[RightAscension](RightAscension.fromStringHMS.reverseGet)

  implicit val decEncoder: Encoder[Declination] =
    Encoder.encodeString
      .contramap[Declination](Declination.fromStringSignedDMS.reverseGet)

  implicit val siderealTargetEncoder: Encoder[SiderealTarget] = new Encoder[SiderealTarget] {
    override def apply(target: SiderealTarget): Json =
      Json.obj(
        "id"          -> target.id.asJson,
        "name"        -> target.name.asJson,
        "object_type" -> (TargetType.Sidereal: TargetType).asJson,
        "ra"          -> targetRA.get(target).asJson,
        "dec"         -> targetDec.get(target).asJson
      )
  }

}
