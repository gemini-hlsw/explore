// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.events

import boopickle.DefaultBasic.*
import boopickle.Pickler
import explore.model.boopickle.CatalogPicklers.given
import lucuma.ags.AgsAnalysis
import lucuma.ags.AgsParams
import lucuma.ags.AgsPosition
import lucuma.ags.GuideStarCandidate
import lucuma.core.math.Coordinates
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Target
import workers.WorkerRequest
import cats.data.NonEmptyList

object AgsMessage {
  case class Request(
    id:                 Target.Id,
    constraints:        ConstraintSet,
    wavelength:         Wavelength,
    baseCoordinates:    Coordinates,
    scienceCoordinates: List[Coordinates],
    positions:          NonEmptyList[AgsPosition],
    params:             AgsParams,
    candidates:         List[GuideStarCandidate]
  ) extends WorkerRequest {
    type ResponseType = List[AgsAnalysis]
  }

  given Pickler[Request] = generatePickler
}
