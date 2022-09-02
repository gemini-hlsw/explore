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
import lucuma.ags.GuideProbe
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

// Boopicklers for catalog related types
trait CatalogPicklers extends CommonPicklers {

  given Pickler[Epoch] =
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

  given Pickler[ProperMotion.RA] =
    transformPickler(ProperMotion.RA.microarcsecondsPerYear.reverseGet)(
      ProperMotion.RA.microarcsecondsPerYear.get
    )

  given Pickler[ProperMotion.Dec] =
    transformPickler(ProperMotion.Dec.microarcsecondsPerYear.reverseGet)(
      ProperMotion.Dec.microarcsecondsPerYear.get
    )

  given Pickler[ProperMotion] =
    transformPickler(Function.tupled(ProperMotion.apply _))(x => (x.ra, x.dec))

  given Pickler[Parallax] =
    transformPickler(Parallax.fromMicroarcseconds)(_.μas.value.value)

  given Pickler[SiderealTracking] =
    transformPickler(Function.tupled(SiderealTracking.apply _))(x =>
      (x.baseCoordinates, x.epoch, x.properMotion, x.radialVelocity, x.parallax)
    )

  given Pickler[GuideStarCandidate] =
    transformPickler(Function.tupled(GuideStarCandidate.apply _))(x =>
      (x.id, x.tracking, x.gBrightness)
    )

  given Pickler[AgsPosition] =
    transformPickler(Function.tupled(AgsPosition.apply _))(x => (x.posAngle, x.offsetPos))

  given Pickler[AgsParams.GmosAgsParams] =
    transformPickler(Function.tupled(AgsParams.GmosAgsParams.apply _))(x => (x.fpu, x.port))

  given Pickler[AgsParams] =
    compositePickler[AgsParams]
      .addConcreteType[AgsParams.GmosAgsParams]

  given Pickler[AgsAnalysis.ProperMotionNotAvailable] =
    transformPickler(AgsAnalysis.ProperMotionNotAvailable.apply)(_.target)

  given Pickler[AgsAnalysis.VignettesScience] =
    transformPickler(AgsAnalysis.VignettesScience.apply)(_.target)

  given Pickler[AgsAnalysis.NoGuideStarForProbe] =
    transformPickler(Function.tupled(AgsAnalysis.NoGuideStarForProbe.apply _))(x =>
      (x.guideProbe, x.target)
    )

  given Pickler[AgsAnalysis.MagnitudeTooFaint] =
    transformPickler(Function.tupled(AgsAnalysis.MagnitudeTooFaint.apply _))(x =>
      (x.guideProbe, x.target, x.showGuideSpeed)
    )

  given Pickler[AgsAnalysis.MagnitudeTooBright] =
    transformPickler(Function.tupled(AgsAnalysis.MagnitudeTooBright.apply _))(x =>
      (x.guideProbe, x.target)
    )

  given Pickler[AgsAnalysis.NotReachable] =
    transformPickler(Function.tupled(AgsAnalysis.NotReachable.apply _))(x =>
      (x.position, x.guideProbe, x.target)
    )

  given Pickler[AgsAnalysis.NoMagnitudeForBand] =
    transformPickler(Function.tupled(AgsAnalysis.NoMagnitudeForBand.apply _))(x =>
      (x.guideProbe, x.target)
    )

  given Pickler[Area] =
    transformPickler((x: Long) =>
      Area.fromMicroarcsecondsSquared.getOption(x).getOrElse(sys.error("Cannot unpickle"))
    )(
      _.toMicroarcsecondsSquared
    )

  given Pickler[AgsAnalysis.Usable] =
    transformPickler(Function.tupled(AgsAnalysis.Usable.apply _))(x =>
      (x.guideProbe, x.target, x.guideSpeed, x.quality, x.vignettingArea)
    )

  given Pickler[AgsAnalysis] =
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
