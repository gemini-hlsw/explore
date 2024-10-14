// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import lucuma.core.model.ObjectTracking
import lucuma.core.model.Observation
import lucuma.core.model.Target
import cats.data.NonEmptyMap
import lucuma.core.util.NewType
import lucuma.ui.reusability.given
import scala.collection.immutable.SortedMap
import cats.Order

// Can wrap data for a target or an asterism.
case class ElevationPlotSeries(
  name:     NonEmptyString,
  tracking: ObjectTracking,
  style:    ElevationPlotSeries.Style
) derives Eq

object ElevationPlotSeries:
  object Id extends NewType[Either[Observation.Id, Target.Id]]:
    given Order[Id] = Order.by(_.value)
  type Id = Id.Type

  enum Style derives Eq:
    case Solid, Dashed

  given Reusability[ElevationPlotSeries] = Reusability.byEq

object ElevationPlotData extends NewType[NonEmptyMap[ElevationPlotSeries.Id, ElevationPlotSeries]]:
  given Reusability[ElevationPlotData] =
    Reusability.by[Type, Map[ElevationPlotSeries.Id, ElevationPlotSeries]](
      _.value.toSortedMap.unsorted
    )(using Reusability.map)
type ElevationPlotData = ElevationPlotData.Type
