// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.implicits._
import coulomb._
import explore.model.enum._
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.HCursor
import io.circe.generic.semiauto
import lucuma.core.enum.MagnitudeBand
import lucuma.core.enum.MagnitudeSystem
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.MagnitudeValue
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.RightAscension
import lucuma.core.math.units.CentimetersPerSecond
import lucuma.core.model.Magnitude
import lucuma.core.model.SiderealTracking

object decoders {
  implicit val epochDecoder: Decoder[Epoch] =
    Decoder.decodeString.emap(e =>
      Epoch.fromString.getOption(e).toRight(s"Invalid epoch value: $e")
    )

  val rvmsDecoder: Decoder[RadialVelocity] =
    Decoder.decodeBigDecimal.emap(x =>
      RadialVelocity(x.withUnit[CentimetersPerSecond]).toRight(s"Invalid radial velocity $x")
    )

  implicit val rvDecoder: Decoder[RadialVelocity] = new Decoder[RadialVelocity] {
    final def apply(c: HCursor): Decoder.Result[RadialVelocity] =
      c.downField("centimetersPerSecond").as[RadialVelocity](rvmsDecoder)
  }

  val pxµasDecoder: Decoder[Parallax] =
    Decoder.decodeLong.map(Parallax.fromMicroarcseconds)

  implicit val pxDecoder: Decoder[Parallax] = new Decoder[Parallax] {
    final def apply(c: HCursor): Decoder.Result[Parallax] =
      c.downField("microarcseconds").as[Parallax](pxµasDecoder)
  }

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

  val pmraµasDecoder: Decoder[ProperMotion.RA] =
    Decoder.decodeLong
      .map(ProperMotion.RA.microarcsecondsPerYear.reverseGet)

  implicit val pmraDecoder: Decoder[ProperMotion.RA] = new Decoder[ProperMotion.RA] {
    final def apply(c: HCursor): Decoder.Result[ProperMotion.RA] =
      c.downField("microarcsecondsPerYear").as[ProperMotion.RA](pmraµasDecoder)
  }

  val pmdecµasDecoder: Decoder[ProperMotion.Dec] =
    Decoder.decodeLong
      .map(ProperMotion.Dec.microarcsecondsPerYear.reverseGet)

  implicit val pmdecDecoder: Decoder[ProperMotion.Dec] = new Decoder[ProperMotion.Dec] {
    final def apply(c: HCursor): Decoder.Result[ProperMotion.Dec] =
      c.downField("microarcsecondsPerYear").as[ProperMotion.Dec](pmdecµasDecoder)
  }

  implicit val pmDecoder: Decoder[ProperMotion] = semiauto.deriveDecoder[ProperMotion]

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

  implicit val siderealTrackingDecoder = new Decoder[SiderealTracking] {
    final def apply(c: HCursor): Decoder.Result[SiderealTracking] =
      for {
        bc  <- c.downField("coordinates").as[Coordinates]
        ep  <- c.downField("epoch").as[Epoch]
        pm  <- c.downField("properVelocity").as[Option[ProperMotion]]
        rv  <- c.downField("radialVelocity").as[Option[RadialVelocity]]
        par <- c.downField("parallax").as[Option[Parallax]]
      } yield SiderealTracking(none, bc, ep, pm, rv, par)
  }

  implicit val magnitudeValueDecoder: Decoder[MagnitudeValue] = new Decoder[MagnitudeValue] {
    final def apply(c: HCursor): Decoder.Result[MagnitudeValue] =
      c.as[BigDecimal]
        .map(MagnitudeValue.fromBigDecimal.getOption)
        .flatMap(_.toRight(DecodingFailure("Invalid MagnitudeValue", c.history)))
  }

  implicit val magnitudeDecoder: Decoder[Magnitude] = new Decoder[Magnitude] {
    final def apply(c: HCursor): Decoder.Result[Magnitude] =
      for {
        v <- c.downField("value").as[MagnitudeValue]
        b <- c.downField("band").as[MagnitudeBand]
        s <- c.downField("system").as[MagnitudeSystem]
      } yield Magnitude(v, b, s)
  }
}
