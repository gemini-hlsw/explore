// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.syntax.all._
import eu.timepit.refined._
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.CatalogResults
import explore.model.GuideStarCandidate
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.RightAscension
import lucuma.core.model.SiderealTracking

import scala.scalajs.js

trait InternalEncoders {
  implicit val raEncoder: Encoder[RightAscension]                              =
    Encoder.encodeString.contramap[RightAscension](RightAscension.fromStringHMS.reverseGet)
  implicit val raDecoder: Decoder[RightAscension]                              =
    Decoder.decodeString.emap(v =>
      Either.fromOption(RightAscension.fromStringHMS.getOption(v), "Cannot parse")
    )
  implicit val decEncoder: Encoder[Declination]                                =
    Encoder.encodeString.contramap[Declination](Declination.fromStringSignedDMS.reverseGet)
  implicit val decDecoder: Decoder[Declination]                                =
    Decoder.decodeString.emap(v =>
      Either.fromOption(Declination.fromStringSignedDMS.getOption(v), "Cannot parse")
    )
  implicit val coordinatesEncoder: Encoder[Coordinates]                        = deriveEncoder[Coordinates]
  implicit val coordinatesDecoder: Decoder[Coordinates]                        = deriveDecoder[Coordinates]
  implicit val epochDecoder: Decoder[Epoch]                                    =
    Decoder.decodeString.emap(v => Either.fromOption(Epoch.fromString.getOption(v), "Cannot parse"))
  implicit val epochEncoder: Encoder[Epoch]                                    =
    Encoder.encodeString.contramap[Epoch](Epoch.fromString.reverseGet)
  implicit def avEncoder[A]: Encoder[ProperMotion.AngularVelocityComponent[A]] =
    Encoder.encodeLong.contramap[ProperMotion.AngularVelocityComponent[A]](_.μasy.value)
  implicit def avDecoder[A]: Decoder[ProperMotion.AngularVelocityComponent[A]] =
    Decoder.decodeLong.emap(v =>
      ProperMotion.AngularVelocityComponent.microarcsecondsPerYear[A].get(v).asRight
    )

  implicit val properMotionEncoder: Encoder[ProperMotion] =
    Encoder.forProduct2("ra", "dec")(x => (x.ra, x.dec))
  implicit val properMotionDecoder: Decoder[ProperMotion] =
    Decoder.forProduct2("ra", "dec")(ProperMotion.apply)
  implicit val rvDecoder: Decoder[RadialVelocity]         =
    Decoder.decodeBigDecimal.emap(v =>
      Either.fromOption(RadialVelocity.fromMetersPerSecond.getOption(v), "Cannot parse")
    )
  implicit val rvEncoder: Encoder[RadialVelocity]         =
    Encoder.encodeBigDecimal.contramap[RadialVelocity](_.rv.value)
  implicit val pxDecoder: Decoder[Parallax]               =
    Decoder.decodeLong.emap(v => Parallax.fromMicroarcseconds(v).asRight)
  implicit val pxEncoder: Encoder[Parallax]               =
    Encoder.encodeLong.contramap[Parallax](_.μas.value.value)
  implicit val trackingDecoder: Decoder[SiderealTracking] = deriveDecoder[SiderealTracking]
  implicit val trackingEncoder: Encoder[SiderealTracking] = deriveEncoder[SiderealTracking]
  implicit val nesDecoder: Decoder[NonEmptyString]        =
    Decoder.decodeString.emap[NonEmptyString](refineV[NonEmpty](_))
  implicit val nesEncoder: Encoder[NonEmptyString]        =
    Encoder.encodeString.contramap[NonEmptyString](_.value)
  implicit val targetDecoder: Decoder[GuideStarCandidate] = deriveDecoder[GuideStarCandidate]
  implicit val targetEncoder: Encoder[GuideStarCandidate] = deriveEncoder[GuideStarCandidate]
  implicit val resultsDecoder: Decoder[CatalogResults]    = deriveDecoder[CatalogResults]
  implicit val resultsEncoder: Encoder[CatalogResults]    = deriveEncoder[CatalogResults]
}

package object events extends InternalEncoders {
  val LogoutEvent         = 1
  val CatalogRequestEvent = 2

  // These are messages sent across tabs thus they need to be JS compatible
  // We don't need yet more than just an index to  differentiate
  sealed trait ExploreEvent extends js.Object {
    def event: Int
    def value: js.Any // encode whatever value as a String. it can be e.g. json
  }

  object ExploreEvent {
    class Logout(val nonce: Long) extends ExploreEvent {
      val event = LogoutEvent
      val value = nonce.toString
    }

    object Logout {
      val event                          = LogoutEvent
      def apply(nonce: Long)             = new Logout(nonce)
      def unapply(l: Logout): Some[Long] = Some(l.nonce)
    }

    class CatalogRequest(q: SiderealTracking) extends ExploreEvent {
      val event = CatalogRequestEvent
      val value = q.asJson.noSpaces
    }

    object CatalogRequest {
      val event                      = CatalogRequestEvent
      def apply(t: SiderealTracking) = new CatalogRequest(t)
    }
  }
}
