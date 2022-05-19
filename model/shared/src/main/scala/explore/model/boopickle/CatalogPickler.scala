// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import boopickle.DefaultBasic._
import lucuma.core.math.Coordinates

import eu.timepit.refined._
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.collection.NonEmpty
import lucuma.core.model.SiderealTracking
import lucuma.core.math.RightAscension
import lucuma.core.math.Angle
import lucuma.core.math.HourAngle
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.ProperMotion
import lucuma.core.math.Parallax
import lucuma.core.math.RadialVelocity

// Boopicklers for catalog related types
trait CatalogPicklers {

  implicit def picklerNonEmptyString: Pickler[NonEmptyString] =
    new Pickler[NonEmptyString] {
      override def pickle(a: NonEmptyString)(implicit state: PickleState): Unit = {
        state.pickle(a.value)
        ()
      }
      override def unpickle(implicit state: UnpickleState): NonEmptyString      = {
        val str = state.unpickle[String]
        refineV[NonEmpty](str).getOrElse(sys.error("Cannot unpickle"))
      }
    }

  implicit def picklerAngle: Pickler[Angle] =
    transformPickler(Angle.fromMicroarcseconds)(_.toMicroarcseconds)

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

  implicit def picklerGuideStarCandadite: Pickler[GuideStarCandidate] =
    transformPickler(Function.tupled(GuideStarCandidate.apply _))(x =>
      (x.name, x.tracking, x.gBrightness)
    )

  implicit def picklerCatalogResults: Pickler[CatalogResults] =
    transformPickler(CatalogResults.apply)(_.candidates)
}

object CatalogPicklers extends CatalogPicklers
