// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import cats.syntax.all._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval
import io.circe.Decoder
import io.circe.Encoder
import io.circe._
import io.circe.generic.semiauto._
import io.circe.refined._
import lucuma.core.optics.SplitEpi
import monocle.Focus
import monocle.Prism
import monocle.macros.GenPrism

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

sealed trait ElevationRange extends Product with Serializable

object ElevationRange {
  val airmass: Prism[ElevationRange, AirMassRange] =
    GenPrism[ElevationRange, AirMassRange]

  val hourAngle: Prism[ElevationRange, HourAngleRange] =
    GenPrism[ElevationRange, HourAngleRange]

  implicit val ElevationRangeEq: Eq[ElevationRange] = Eq.fromUniversalEquals

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
}

final case class AirMassRange protected (
  min: AirMassRange.DecimalValue,
  max: AirMassRange.DecimalValue
) extends ElevationRange

object AirMassRange extends AirmassRangeOptics {
  val MinValue = BigDecimal(1.0)
  val MaxValue = BigDecimal(3.0)
  type Value        = Interval.Closed[MinValue.type, MaxValue.type]
  type DecimalValue = BigDecimal Refined Value

  val DefaultMin: DecimalValue =
    Refined.unsafeApply[BigDecimal, Value](BigDecimal(1.0))
  val DefaultMax: DecimalValue =
    Refined.unsafeApply[BigDecimal, Value](BigDecimal(2.0))
  val Default: AirMassRange    = apply(DefaultMin, DefaultMax)

  implicit val airmassRangeDecoder: Decoder[AirMassRange] = deriveDecoder
  implicit val airmassRangeEncoder: Encoder[AirMassRange] = deriveEncoder
  implicit val airmassRangeEq: Eq[AirMassRange]           = Eq.by(ar => (ar.min.value, ar.max.value))
}

trait AirmassRangeOptics {
  import AirMassRange.DecimalValue
  val min = Focus[AirMassRange](_.min)
  val max = Focus[AirMassRange](_.max)

  /** @group Optics */
  // Ensures that min <= max by swapping if necessary
  lazy val fromDecimalValues: SplitEpi[(DecimalValue, DecimalValue), AirMassRange] =
    SplitEpi(
      t => {
        val (min, max) = t
        if (min.value <= max.value) AirMassRange(min, max)
        else AirMassRange(max, min)
      },
      a => (a.min, a.max)
    )
}

final case class HourAngleRange protected (
  minHours: HourAngleRange.DecimalHour,
  maxHours: HourAngleRange.DecimalHour
) extends ElevationRange

object HourAngleRange extends HourAngleRangeOptics {
  val MinHour = BigDecimal(-5.0)
  val MaxHour = BigDecimal(5.0)
  type Hour        = Interval.Closed[MinHour.type, MaxHour.type]
  type DecimalHour = BigDecimal Refined Hour

  val DefaultMin = Refined.unsafeApply[BigDecimal, Hour](MinHour)
  val DefaultMax = Refined.unsafeApply[BigDecimal, Hour](MaxHour)

  val Default = HourAngleRange(DefaultMin, DefaultMax)

  implicit val hourAngleRangeDecoder: Decoder[HourAngleRange] = deriveDecoder
  implicit val hourAngleRangeEncoder: Encoder[HourAngleRange] = deriveEncoder
  implicit val hourAngleRangeEq: Eq[HourAngleRange]           =
    Eq.by(hr => (hr.minHours.value, hr.maxHours.value))
}

trait HourAngleRangeOptics {
  import HourAngleRange.DecimalHour
  val minHours = Focus[HourAngleRange](_.minHours)
  val maxHours = Focus[HourAngleRange](_.maxHours)

  /** @group Optics */
  // Ensures that minHours <= maxHours by swapping if necessary
  lazy val fromDecimalHours: SplitEpi[(DecimalHour, DecimalHour), HourAngleRange] =
    SplitEpi(
      t => {
        val (min, max) = t
        if (min.value <= max.value) HourAngleRange(min, max)
        else HourAngleRange(max, min)
      },
      a => (a.minHours, a.maxHours)
    )
}
