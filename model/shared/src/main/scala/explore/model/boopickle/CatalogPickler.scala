// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.boopickle

import boopickle.DefaultBasic._
import cats.Order
import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import coulomb._
import coulomb.syntax.*
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.Validate
import lucuma.ags.AgsAnalysis
import lucuma.ags.AgsParams
import lucuma.ags.AgsPosition
import lucuma.ags.GuideStarCandidate
import lucuma.core.geom.Area
import lucuma.core.math.Angle
import lucuma.core.math.Axis
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.HourAngle
import lucuma.core.math.Offset
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.RightAscension
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
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

  given Pickler[Target.Id] =
    transformPickler(Target.Id.apply)(_.value)

  given picklerNonEmptyList[A: Pickler]: Pickler[NonEmptyList[A]] =
    transformPickler(NonEmptyList.fromListUnsafe[A])(_.toList)

  given picklerNonEmptyMap[A: Order: Pickler, B: Pickler]: Pickler[NonEmptyMap[A, B]] =
    transformPickler((a: NonEmptyList[(A, B)]) => NonEmptyMap.of(a.head, a.tail: _*))(
      _.toNel
    )

  given Pickler[Wavelength] =
    transformPickler((i: Int) =>
      Wavelength.fromPicometers
        .getOption(i)
        .getOrElse(sys.error("cannot unpickle"))
    )(_.toPicometers.value.value)

  given Pickler[Angle] =
    transformPickler(Angle.fromMicroarcseconds)(_.toMicroarcseconds)

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

  given Pickler[Coordinates] =
    transformPickler(Function.tupled(Coordinates.apply _))(x => (x.ra, x.dec))

  given Pickler[Offset] =
    transformPickler((x: (Angle, Angle)) =>
      Offset(Offset.Component.angle[Axis.P].reverseGet(x._1),
             Offset.Component.angle[Axis.Q].reverseGet(x._2)
      )
    )(x => (x.p.toAngle, x.q.toAngle))
}

// Boopicklers for catalog related types
trait CatalogPicklers extends CommonPicklers {

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

  implicit def picklerAgsPosition: Pickler[AgsPosition] =
    transformPickler(Function.tupled(AgsPosition.apply _))(x => (x.posAngle, x.offsetPos))

  implicit def picklerAgsParamsGmos: Pickler[AgsParams.GmosAgsParams] =
    transformPickler(Function.tupled(AgsParams.GmosAgsParams.apply _))(x => (x.fpu, x.port))

  implicit def picklerAgsParams: Pickler[AgsParams] =
    compositePickler[AgsParams]
      .addConcreteType[AgsParams.GmosAgsParams]

  implicit def picklerProperMotionNotAvailable: Pickler[AgsAnalysis.ProperMotionNotAvailable] =
    transformPickler(AgsAnalysis.ProperMotionNotAvailable.apply)(_.target)

  implicit def picklerVignettesScience: Pickler[AgsAnalysis.VignettesScience] =
    transformPickler(AgsAnalysis.VignettesScience.apply)(_.target)

  implicit def picklerNoGuideStarForProbe: Pickler[AgsAnalysis.NoGuideStarForProbe] =
    transformPickler(Function.tupled(AgsAnalysis.NoGuideStarForProbe.apply _))(x =>
      (x.guideProbe, x.target)
    )

  implicit def picklerMagnitudeTooFaint: Pickler[AgsAnalysis.MagnitudeTooFaint] =
    transformPickler(Function.tupled(AgsAnalysis.MagnitudeTooFaint.apply _))(x =>
      (x.guideProbe, x.target, x.showGuideSpeed)
    )

  implicit def picklerMagnitudeTooBright: Pickler[AgsAnalysis.MagnitudeTooBright] =
    transformPickler(Function.tupled(AgsAnalysis.MagnitudeTooBright.apply _))(x =>
      (x.guideProbe, x.target)
    )

  implicit def picklerNotReachable: Pickler[AgsAnalysis.NotReachable] =
    transformPickler(Function.tupled(AgsAnalysis.NotReachable.apply _))(x =>
      (x.position, x.guideProbe, x.target)
    )

  implicit def picklerNoMagnitudeForBand: Pickler[AgsAnalysis.NoMagnitudeForBand] =
    transformPickler(Function.tupled(AgsAnalysis.NoMagnitudeForBand.apply _))(x =>
      (x.guideProbe, x.target)
    )

  implicit def picklerArea: Pickler[Area] =
    transformPickler((x: Long) =>
      Area.fromMicroarcsecondsSquared.getOption(x).getOrElse(sys.error("Cannot unpickle"))
    )(
      _.toMicroarcsecondsSquared
    )

  implicit def picklerUsable: Pickler[AgsAnalysis.Usable] =
    transformPickler(Function.tupled(AgsAnalysis.Usable.apply _))(x =>
      (x.guideProbe, x.target, x.guideSpeed, x.quality, x.vignettingArea)
    )

  implicit def picklerAgsAnalysis: Pickler[AgsAnalysis] =
    compositePickler[AgsAnalysis]
      .addConcreteType[AgsAnalysis.ProperMotionNotAvailable]
      .addConcreteType[AgsAnalysis.VignettesScience]
      .addConcreteType[AgsAnalysis.NoGuideStarForProbe]
      .addConcreteType[AgsAnalysis.MagnitudeTooFaint]
      .addConcreteType[AgsAnalysis.MagnitudeTooBright]
      .addConcreteType[AgsAnalysis.NotReachable]
      .addConcreteType[AgsAnalysis.NoMagnitudeForBand]
      .addConcreteType[AgsAnalysis.Usable]
}

object CatalogPicklers extends CatalogPicklers
