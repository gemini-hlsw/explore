// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import explore.model.enum._
import io.circe.Decoder
import io.circe.HCursor
import io.circe.generic.semiauto
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension

object decoders {
  val raµasDecoder: Decoder[RightAscension] =
    Decoder.decodeLong
      .map(
        (RightAscension.fromAngleExact.getOption _).compose(Angle.fromMicroarcseconds _)
      )
      .map(_.getOrElse(RightAscension.Zero))

  implicit val raDecoder: Decoder[RightAscension] = new Decoder[RightAscension] {
    final def apply(c: HCursor): Decoder.Result[RightAscension] =
      c.downField("microarcseconds").as[RightAscension](raµasDecoder)
  }

  val decµasDecoder: Decoder[Declination] =
    Decoder.decodeLong
      .map(
        (Declination.fromAngle.getOption _).compose(Angle.fromMicroarcseconds _)
      )
      .emap(_.toRight("Invalid µarcsec value for declination"))

  implicit val decDecoder: Decoder[Declination] = new Decoder[Declination] {
    final def apply(c: HCursor): Decoder.Result[Declination] =
      c.downField("microarcseconds").as[Declination](decµasDecoder)
  }

  implicit val coordDecoder: Decoder[Coordinates] = semiauto.deriveDecoder[Coordinates]

  implicit val constraintsDecoder = new Decoder[Constraints] {
    final def apply(c: HCursor): Decoder.Result[Constraints] =
      for {
        id   <- c.downField("id").as[SiderealTarget.Id]
        name <- c.downField("name").as[String]
        cc   <- c.downField("cloud_cover").as[CloudCover]
        iq   <- c.downField("image_quality").as[ImageQuality]
        sb   <- c.downField("sky_background").as[SkyBackground]
        wv   <- c.downField("water_vapor").as[WaterVapor]
      } yield Constraints(id, name, cc, iq, sb, wv)
  }
}
