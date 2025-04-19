// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.boopickle

import boopickle.DefaultBasic.*
import coulomb.*
import eu.timepit.refined.*
import eu.timepit.refined.api.Refined
import lucuma.ags.AgsAnalysis
import lucuma.ags.AgsParams
import lucuma.ags.AgsPosition
import lucuma.ags.GuideStarCandidate
import lucuma.core.enums.GuideProbe
import lucuma.core.geom.Area
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Epoch
import lucuma.core.math.Offset
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.model.ObjectTracking
import lucuma.core.model.SiderealTracking

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
    transformPickler(ProperMotion.RA.microarcsecondsPerYear.get)(
      ProperMotion.RA.microarcsecondsPerYear.reverseGet
    )

  given Pickler[ProperMotion.Dec] =
    transformPickler(ProperMotion.Dec.microarcsecondsPerYear.get)(
      ProperMotion.Dec.microarcsecondsPerYear.reverseGet
    )

  given Pickler[ProperMotion] = generatePickler

  given Pickler[Parallax] =
    transformPickler(Parallax.fromMicroarcseconds)(_.μas.value.value)

  given Pickler[SiderealTracking] = generatePickler

  given Pickler[GuideStarCandidate] = generatePickler

  given Pickler[AgsPosition] = generatePickler

  given Pickler[AgsParams.GmosAgsParams] = generatePickler

  given Pickler[AgsParams] =
    compositePickler[AgsParams]
      .addConcreteType[AgsParams.GmosAgsParams]

  given Pickler[Area] =
    transformPickler((x: Long) =>
      Area.fromMicroarcsecondsSquared.getOption(x).getOrElse(sys.error("Cannot unpickle"))
    )(
      _.toMicroarcsecondsSquared
    )

  given Pickler[AgsAnalysis.Usable] = generatePickler

  given Pickler[ObjectTracking.SiderealObjectTracking] = generatePickler

  given Pickler[ObjectTracking.SiderealAsterismTracking] = generatePickler

  given Pickler[ObjectTracking.ConstantTracking] = generatePickler

  given Pickler[ObjectTracking] =
    compositePickler[ObjectTracking]
      .addConcreteType[ObjectTracking.SiderealObjectTracking]
      .addConcreteType[ObjectTracking.SiderealAsterismTracking]
      .addConcreteType[ObjectTracking.ConstantTracking]
}

object CatalogPicklers extends CatalogPicklers
