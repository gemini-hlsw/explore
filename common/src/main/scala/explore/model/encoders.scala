// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import gsp.math.Declination
import gsp.math.RightAscension
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax._

import ModelOptics._

object encoders {

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
        "object_type" -> "Sidereal".asJson,
        "ra"          -> targetRA.get(target).asJson,
        "dec"         -> targetDec.get(target).asJson
      )

  }

}
