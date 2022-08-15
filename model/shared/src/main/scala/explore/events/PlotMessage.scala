// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.events

import boopickle.DefaultBasic._
import boopickle.Pickler
import explore.model.boopickle.CatalogPicklers.given
import explore.model.boopickle.CommonPicklers
import lucuma.ags.AgsAnalysis
import lucuma.ags.AgsParams
import lucuma.ags.AgsPosition
import lucuma.ags.GuideStarCandidate
import lucuma.core.enums.Site
import lucuma.core.math.Coordinates
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Semester
import lucuma.core.model.Target
import workers.WorkerRequest

object PlotMessage extends CommonPicklers {

  sealed trait Request extends WorkerRequest

  // We return `Long`s instead `Instant` and `Duration` in order to offload as much of the
  // work as possible to the worker. The chart will need the numeric values to plot.
  final case class SemesterPoint(epochMilli: Long, visibilityMillis: Long)
  object SemesterPoint {
    given Pickler[SemesterPoint] = generatePickler
  }

  final case class RequestSemesterSidereal(
    semester: Semester,
    site:     Site,
    coords:   Coordinates,
    dayRate:  Long
  ) extends Request {
    type ResponseType = SemesterPoint
  }

  private given Pickler[RequestSemesterSidereal] = generatePickler

  given Pickler[Request] = generatePickler
}
