// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import cats.syntax.all._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.JsonObject
import io.circe.refined._
import io.circe.syntax._
import lucuma.core.enum._
import lucuma.core.model.ConstraintSet
import io.circe.DecodingFailure
import monocle.macros.Lenses

@Lenses
final case class ConstraintSetModel(
  id:              ConstraintSet.Id,
  name:            NonEmptyString,
  imageQuality:    ImageQuality,
  cloudExtinction: CloudExtinction,
  skyBackground:   SkyBackground,
  waterVapor:      WaterVapor,
  elevationRange:  ElevationRange
)

object ConstraintSetModel {
  implicit val eqConstraintSetModel: Eq[ConstraintSetModel] =
    Eq.by(cs =>
      (cs.id,
       cs.name,
       cs.imageQuality,
       cs.cloudExtinction,
       cs.skyBackground,
       cs.waterVapor,
       cs.elevationRange
      )
    )

  implicit val constraintSetEncoder: Encoder[ConstraintSetModel] = new Encoder[ConstraintSetModel] {
    def apply(cs: ConstraintSetModel): Json = {

      val elevationRange: JsonObject = cs.elevationRange match {
        case AirMassRange(min, max)             =>
          JsonObject(("type", ElevationRangeType.AirMassRange.asJson),
                     ("min", min.asJson),
                     ("max", max.asJson)
          )
        case HourAngleRange(minHours, maxHours) =>
          JsonObject(("type", ElevationRangeType.HourAngleRange.asJson),
                     ("minHours", minHours.asJson),
                     ("maxHours", maxHours.asJson)
          )
      }

      JsonObject(
        ("id", cs.id.asJson),
        ("name", cs.name.asJson),
        ("imageQuality", cs.imageQuality.asJson),
        ("cloudExtinction", cs.cloudExtinction.asJson),
        ("skyBackground", cs.skyBackground.asJson),
        ("waterVapor", cs.waterVapor.asJson),
        ("elevationRange", elevationRange.asJson)
      ).asJson
    }
  }

  implicit val elevationRangeDecoder: Decoder[ElevationRange] = new Decoder[ElevationRange] {
    final def apply(c: HCursor): Decoder.Result[ElevationRange] =
      c.downField("type").as[ElevationRangeType].flatMap {
        case ElevationRangeType.AirMassRange   =>
          for {
            min <- c.downField("min").as[AirMassRange.DecimalValue]
            max <- c.downField("max").as[AirMassRange.DecimalValue]
          } yield AirMassRange.fromDecimalValues.get((min, max))
        case ElevationRangeType.HourAngleRange =>
          for {
            min <- c.downField("minHours").as[HourAngleRange.DecimalHour]
            max <- c.downField("maxHours").as[HourAngleRange.DecimalHour]
          } yield HourAngleRange.fromDecimalHours.get((min, max))
      }
  }

  implicit val constraintSetDecoder: Decoder[ConstraintSetModel] = new Decoder[ConstraintSetModel] {
    final def apply(c: HCursor): Decoder.Result[ConstraintSetModel] =
      for {
        id   <- c.downField("id").as[ConstraintSet.Id]
        name <- c.downField("name").as[NonEmptyString]
        iq   <- c.downField("imageQuality").as[ImageQuality]
        ce   <- c.downField("cloudExtinction").as[CloudExtinction]
        sb   <- c.downField("skyBackground").as[SkyBackground]
        wv   <- c.downField("waterVapor").as[WaterVapor]
        er   <- c.downField("elevationRange").as[ElevationRange]
      } yield ConstraintSetModel(id, name, iq, ce, sb, wv, er)
  }

  private sealed abstract class ElevationRangeType(val typeName: String) { self =>
    implicit val elevationRangeTypeEncoder: Encoder[self.type] =
      Encoder.encodeString.contramap(_.typeName)
  }

  private object ElevationRangeType {
    case object AirMassRange   extends ElevationRangeType("AirMassRange")
    case object HourAngleRange extends ElevationRangeType("HourAngleRange")

    implicit val elevationRangeTypeDecoder: Decoder[ElevationRangeType] =
      Decoder.instance { c =>
        c.as[String]
          .flatMap(_ match {
            case AirMassRange.typeName   => AirMassRange.asRight
            case HourAngleRange.typeName => HourAngleRange.asRight
            case _                       => DecodingFailure("Unsupported elevation range type", c.history).asLeft
          })
      }
  }
}
