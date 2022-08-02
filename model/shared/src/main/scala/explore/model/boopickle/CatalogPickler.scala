// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.boopickle

import boopickle.DefaultBasic._
import coulomb._
import coulomb.syntax.*
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.Validate
import lucuma.ags.GuideStarCandidate
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.HourAngle
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.RightAscension
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.SiderealTracking
import lucuma.core.util.Enumerated

trait CommonPicklers {
  implicit def picklerRefined[A: Pickler, B](implicit v: Validate[A, B]): Pickler[A Refined B] =
    new Pickler[A Refined B] {
      override def pickle(a: A Refined B)(implicit state: PickleState): Unit = {
        state.pickle(a.value)
        ()
      }
      override def unpickle(implicit state: UnpickleState): A Refined B      = {
        val value = state.unpickle[A]
        refineV[B](value).getOrElse(sys.error("Cannot unpickle"))
      }
    }

  implicit def picklerQuantity[A: Pickler, B]: Pickler[Quantity[A, B]] =
    new Pickler[Quantity[A, B]] {
      override def pickle(a: Quantity[A, B])(implicit state: PickleState): Unit = {
        state.pickle(a.value)
        ()
      }
      override def unpickle(implicit state: UnpickleState): Quantity[A, B]      = {
        val value = state.unpickle[A]
        value.withUnit[B]
      }
    }

  implicit def picklerEnumeration[A: Enumerated]: Pickler[A] =
    transformPickler((a: String) =>
      Enumerated[A].fromTag(a).getOrElse(sys.error("Cannot unpickle"))
    )(
      Enumerated[A].tag(_)
    )

  implicit def picklerWavelength: Pickler[Wavelength] =
    transformPickler((i: Int) =>
      Wavelength.fromPicometers
        .getOption(i)
        .getOrElse(sys.error("cannot unpickle"))
    )(_.toPicometers.value.value)

  implicit def picklerAngle: Pickler[Angle] =
    transformPickler(Angle.fromMicroarcseconds)(_.toMicroarcseconds)

}

// Boopicklers for catalog related types
trait CatalogPicklers extends CommonPicklers {

  implicit def picklerHourAngle: Pickler[HourAngle] =
    transformPickler(HourAngle.fromMicroseconds)(_.toMicroseconds)

  implicit def picklerRA: Pickler[RightAscension] =
    transformPickler(RightAscension.apply)(_.toHourAngle)

  implicit def picklerDec: Pickler[Declination] =
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

  implicit def picklerCoordinates: Pickler[Coordinates] =
    transformPickler(Function.tupled(Coordinates.apply _))(x => (x.ra, x.dec))

  implicit def picklerEpoch: Pickler[Epoch] =
    new Pickler[Epoch] {
      override def pickle(a: Epoch)(implicit state: PickleState): Unit = {
        state.pickle(a.scheme.prefix)
        state.pickle(a.toMilliyears.value)
        ()
      }
      override def unpickle(implicit state: UnpickleState): Epoch      = {
        val prefix   = state.unpickle[Char]
        val miliyear = state.unpickle[Int]
        (prefix match {
          case 'J' => Epoch.Julian.fromIntMilliyears(miliyear)
          case 'B' => Epoch.Besselian.fromIntMilliyears(miliyear)
          case _   => None
        }).getOrElse(sys.error("Cannot unpickle"))
      }
    }

  implicit def picklerAngularVelocityRA: Pickler[ProperMotion.RA] =
    transformPickler(ProperMotion.RA.microarcsecondsPerYear.reverseGet)(
      ProperMotion.RA.microarcsecondsPerYear.get
    )

  implicit def picklerAngularVelocityDec: Pickler[ProperMotion.Dec] =
    transformPickler(ProperMotion.Dec.microarcsecondsPerYear.reverseGet)(
      ProperMotion.Dec.microarcsecondsPerYear.get
    )
  implicit def picklerProperMotion: Pickler[ProperMotion]           =
    transformPickler(Function.tupled(ProperMotion.apply _))(x => (x.ra, x.dec))

  implicit def picklerRadialVelocity: Pickler[RadialVelocity] =
    transformPickler((x: BigDecimal) =>
      RadialVelocity.fromMetersPerSecond
        .getOption(x)
        .getOrElse(sys.error("Cannot unpickle"))
    )(RadialVelocity.fromMetersPerSecond.reverseGet)

  implicit def picklerParallax: Pickler[Parallax] =
    transformPickler(Parallax.fromMicroarcseconds)(_.μas.value.value)

  implicit def picklerSiderealTracking: Pickler[SiderealTracking] =
    transformPickler(Function.tupled(SiderealTracking.apply _))(x =>
      (x.baseCoordinates, x.epoch, x.properMotion, x.radialVelocity, x.parallax)
    )

  implicit def picklerGuideStarCandidate: Pickler[GuideStarCandidate] =
    transformPickler(Function.tupled(GuideStarCandidate.apply _))(x =>
      (x.id, x.tracking, x.gBrightness)
    )

  implicit def picklerElevationRangeAirMassDecimalValue
    : Pickler[ElevationRange.AirMass.DecimalValue] =
    transformPickler((b: BigDecimal) =>
      ElevationRange.AirMass.DecimalValue.from(b).getOrElse(sys.error("Cannot unpickle"))
    )(_.value)

  implicit def picklerElevationRangeHourAngleDecimalHour
    : Pickler[ElevationRange.HourAngle.DecimalHour] =
    transformPickler((b: BigDecimal) =>
      ElevationRange.HourAngle.DecimalHour.from(b).getOrElse(sys.error("Cannot unpickle"))
    )(_.value)

  implicit def picklerElevationRangeAirMass: Pickler[ElevationRange.AirMass] =
    transformPickler(ElevationRange.AirMass.fromDecimalValues.get)(x => (x.min, x.max))

  implicit def picklerElevationRangeHourAngle: Pickler[ElevationRange.HourAngle] =
    transformPickler(ElevationRange.HourAngle.fromDecimalHours.get)(x => (x.minHours, x.maxHours))

  implicit def picklerElevationRange: Pickler[ElevationRange] =
    compositePickler[ElevationRange]
      .addConcreteType[ElevationRange.AirMass]
      .addConcreteType[ElevationRange.HourAngle]

  implicit def picklerConstraintSet: Pickler[ConstraintSet] =
    transformPickler(Function.tupled(ConstraintSet.apply _))(x =>
      (x.imageQuality, x.cloudExtinction, x.skyBackground, x.waterVapor, x.elevationRange)
    )

}

object CatalogPicklers extends CatalogPicklers
