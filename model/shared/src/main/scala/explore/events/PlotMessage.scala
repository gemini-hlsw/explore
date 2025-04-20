// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.events

import boopickle.DefaultBasic.*
import boopickle.Pickler
import explore.model.boopickle.CommonPicklers
import lucuma.core.enums.Site
import lucuma.core.math.Coordinates
import lucuma.core.model.Semester
import workers.WorkerRequest

object PlotMessage extends CommonPicklers {

  sealed trait Request   extends WorkerRequest
  case object CleanCache extends Request {
    type ResponseType = Unit
  }

  // We return `Long`s instead `Instant` and `Duration` in order to offload as much of the
  // work as possible to the worker. The chart will need the numeric values to plot.
  case class SemesterPoint(epochMilli: Long, visibilityMillis: Long)
  object SemesterPoint {
    given Pickler[SemesterPoint] = generatePickler
  }

  case class RequestSemesterSidereal(
    semester: Semester,
    site:     Site,
    coords:   Coordinates,
    dayRate:  Long
  ) extends Request {
    type ResponseType = SemesterPoint
  }

  private given Pickler[RequestSemesterSidereal] = generatePickler
  private given Pickler[CleanCache.type]         = generatePickler

  given Pickler[Request] = generatePickler
}
