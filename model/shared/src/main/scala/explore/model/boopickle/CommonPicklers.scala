// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.boopickle

import boopickle.DefaultBasic.*
import cats.Order
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import coulomb.*
import coulomb.syntax.*
import eu.timepit.refined.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.Validate
import eu.timepit.refined.numeric.NonNegative
import lucuma.ags.AgsAnalysis
import lucuma.ags.AgsParams
import lucuma.ags.AgsPosition
import lucuma.ags.GuideStarCandidate
import lucuma.core.geom.Area
import lucuma.core.math.Angle
import lucuma.core.math.Axis
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.HourAngle
import lucuma.core.math.Offset
import lucuma.core.math.Parallax
import lucuma.core.math.Place
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.RightAscension
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthRange
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.Semester
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.core.model.given
import lucuma.core.util.Enumerated
import lucuma.core.util.NewType
import lucuma.core.util.TimeSpan
import org.http4s.Uri

import java.time.Duration
import java.time.Instant
import java.time.LocalDate
import java.time.Year
import java.time.ZoneId

trait CommonPicklers {
  given picklerRefined[A: Pickler, B](using Validate[A, B]): Pickler[A Refined B] =
    new Pickler[A Refined B] {
      override def pickle(a: A Refined B)(using state: PickleState): Unit = {
        state.pickle(a.value)
        ()
      }
      override def unpickle(using state: UnpickleState): A Refined B      = {
        val value = state.unpickle[A]
        refineV[B](value).getOrElse(sys.error("Cannot unpickle"))
      }
    }

  given picklerQuantity[A: Pickler, B]: Pickler[Quantity[A, B]] =
    new Pickler[Quantity[A, B]] {
      override def pickle(a: Quantity[A, B])(using state: PickleState): Unit = {
        state.pickle(a.value)
        ()
      }
      override def unpickle(using state: UnpickleState): Quantity[A, B]      = {
        val value = state.unpickle[A]
        value.withUnit[B]
      }
    }

  given picklerEnumeration[A: Enumerated]: Pickler[A] =
    transformPickler((a: String) =>
      Enumerated[A].fromTag(a).getOrElse(sys.error("Cannot unpickle"))
    )(
      Enumerated[A].tag(_)
    )

  def picklerNewType[A: Pickler](nt: NewType[A]): Pickler[nt.Type] =
    transformPickler(nt(_))(_.value)

  given Pickler[Target.Id] = generatePickler

  given picklerNonEmptyList[A: Pickler]: Pickler[NonEmptyList[A]] =
    transformPickler(NonEmptyList.fromListUnsafe[A])(_.toList)

  given picklerNonEmptyChaing[A: Pickler]: Pickler[NonEmptyChain[A]] =
    transformPickler(NonEmptyChain.fromNonEmptyList[A])(_.toNonEmptyList)

  given picklerNonEmptyMap[A: Order: Pickler, B: Pickler]: Pickler[NonEmptyMap[A, B]] =
    transformPickler((a: NonEmptyList[(A, B)]) => NonEmptyMap.of(a.head, a.tail: _*))(
      _.toNel
    )

  given Pickler[Wavelength] =
    transformPickler((i: Int) =>
      Wavelength
        .fromIntPicometers(i)
        .getOrElse(sys.error("cannot unpickle"))
    )(_.toPicometers.value.value)

  given Pickler[WavelengthRange] =
    transformPickler((i: Int) =>
      WavelengthRange
        .fromIntPicometers(i)
        .getOrElse(sys.error("cannot unpickle"))
    )(_.toPicometers.value.value)

  given Pickler[Angle] =
    transformPickler(Angle.fromMicroarcseconds)(_.toMicroarcseconds)

  given Pickler[BrightnessValue] = picklerNewType(BrightnessValue)

  given Pickler[RadialVelocity] =
    transformPickler((x: BigDecimal) =>
      RadialVelocity.fromMetersPerSecond
        .getOption(x)
        .getOrElse(sys.error("Cannot unpickle"))
    )(RadialVelocity.fromMetersPerSecond.reverseGet)

  given Pickler[HourAngle] =
    transformPickler(HourAngle.fromMicroseconds)(_.toMicroseconds)

  given Pickler[RightAscension] =
    transformPickler(RightAscension.apply)(_.toHourAngle)

  given Pickler[Declination] =
    new Pickler[Declination] {
      override def pickle(a: Declination)(implicit state: PickleState): Unit = {
        state.pickle(Declination.fromAngle.reverseGet(a))
        ()
      }
      override def unpickle(implicit state: UnpickleState): Declination      = {
        val angle = state.unpickle[Angle]
        Declination.fromAngle.getOption(angle).getOrElse(sys.error("Cannot unpickle"))
      }
    }

  given Pickler[Coordinates] = generatePickler

  given Pickler[Offset] =
    transformPickler((x: (Angle, Angle)) =>
      Offset(Offset.Component.angle[Axis.P].reverseGet(x._1),
             Offset.Component.angle[Axis.Q].reverseGet(x._2)
      )
    )(x => (x.p.toAngle, x.q.toAngle))

  given Pickler[ElevationRange.AirMass.DecimalValue] =
    transformPickler((b: BigDecimal) =>
      ElevationRange.AirMass.DecimalValue.from(b).getOrElse(sys.error("Cannot unpickle"))
    )(_.value)

  given Pickler[ElevationRange.HourAngle.DecimalHour] =
    transformPickler((b: BigDecimal) =>
      ElevationRange.HourAngle.DecimalHour.from(b).getOrElse(sys.error("Cannot unpickle"))
    )(_.value)

  given ElevationAirMass: Pickler[ElevationRange.AirMass] =
    transformPickler(ElevationRange.AirMass.fromDecimalValues.get)(x => (x.min, x.max))

  given ElevationHourAngle: Pickler[ElevationRange.HourAngle] =
    transformPickler(ElevationRange.HourAngle.fromDecimalHours.get)(x => (x.minHours, x.maxHours))

  given Pickler[ElevationRange] =
    compositePickler[ElevationRange]
      .addConcreteType[ElevationRange.AirMass]
      .addConcreteType[ElevationRange.HourAngle]

  given Pickler[ConstraintSet] = generatePickler

  given Pickler[LocalDate] =
    transformPickler[LocalDate, (Int, Int, Int)]((year, month, dayOfMonth) =>
      LocalDate.of(year, month, dayOfMonth)
    )(d => (d.getYear, d.getMonthValue, d.getDayOfMonth))

  given Pickler[Instant] =
    transformPickler[Instant, (Long, Long)]((epochSecond, nanoOfSecond) =>
      Instant.ofEpochSecond(epochSecond, nanoOfSecond)
    )(i => (i.getEpochSecond, i.getNano))

  given Pickler[Duration] =
    transformPickler(Duration.ofMillis)(_.toMillis)

  given Pickler[TimeSpan] =
    transformPickler(TimeSpan.unsafeFromMicroseconds)(_.toMicroseconds)

  given Pickler[Year] =
    transformPickler(Year.of)(_.getValue)

  given Pickler[ZoneId] =
    transformPickler(ZoneId.of)(_.getId)

  given Pickler[Uri] =
    transformPickler(Uri.unsafeFromString)(_.toString)

  given Pickler[Semester] = generatePickler

  given Pickler[Place] = generatePickler
}

object CommonPicklers extends CommonPicklers
