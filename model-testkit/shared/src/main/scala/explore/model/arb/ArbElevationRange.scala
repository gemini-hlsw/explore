// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import cats.syntax.all._
import explore.model.AirMassRange
import explore.model.ElevationRange
import explore.model.HourAngleRange
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Cogen._
import org.scalacheck.Gen._
// these need to be at the end to avoid diverging implicit expansion problems
import eu.timepit.refined.scalacheck.numeric._

trait ArbElevationRange {
  implicit val airMassRangeArb: Arbitrary[AirMassRange] = Arbitrary {
    for {
      min <- arbitrary[AirMassRange.DecimalValue]
      max <- arbitrary[AirMassRange.DecimalValue]
    } yield AirMassRange.fromDecimalValues.get((min, max))
  }

  implicit val airMassRangeCogen: Cogen[AirMassRange] =
    Cogen[(BigDecimal, BigDecimal)].contramap(amr => (amr.min.value, amr.max.value))

  implicit val hourAngleRangeArb: Arbitrary[HourAngleRange] = Arbitrary {
    for {
      min <- arbitrary[HourAngleRange.DecimalHour]
      max <- arbitrary[HourAngleRange.DecimalHour]
    } yield HourAngleRange.fromDecimalHours.get((min, max))
  }

  implicit val hourAngleRangeCogen: Cogen[HourAngleRange] =
    Cogen[(BigDecimal, BigDecimal)].contramap(har => (har.minHours.value, har.maxHours.value))

  implicit val elevationRangeArb: Arbitrary[ElevationRange] =
    Arbitrary(oneOf(airMassRangeArb.arbitrary, hourAngleRangeArb.arbitrary))

  implicit val elevationRangeCogen: Cogen[ElevationRange] =
    Cogen[Either[AirMassRange, HourAngleRange]].contramap {
      case amr: AirMassRange   => amr.asLeft
      case har: HourAngleRange => har.asRight
    }
}

object ArbElevationRange extends ArbElevationRange
