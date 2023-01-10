// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.Order
import cats.implicits.*
import explore.model.ConstraintGroup
import explore.model.ObsIdSet
import explore.model.ObsSummaryWithTitleAndConf
import explore.model.syntax.all.*
import japgolly.scalajs.react.*
import lucuma.core.model.Observation
import lucuma.ui.reusability.*
import monocle.Focus
import monocle.Getter
import queries.common.ConstraintGroupQueriesGQL.*

import scala.collection.immutable.SortedMap

object ConstraintGroupQueries:
  // The default cats ordering for sorted set sorts by size first, then contents. That's not what we want.
  given Order[ObsIdSet] = Order.by(_.toList)

  type ObservationResult = ConstraintGroupObsQuery.Data.Observations.Matches
  val ObservationResult = ConstraintGroupObsQuery.Data.Observations.Matches

  type ConstraintGroupList = SortedMap[ObsIdSet, ConstraintGroup]
  type ObsList             = SortedMap[Observation.Id, ObsSummaryWithTitleAndConf]

  case class ConstraintSummaryWithObervations(
    constraintGroups: ConstraintGroupList,
    observations:     ObsList
  )

  object ConstraintSummaryWithObervations:
    val constraintGroups = Focus[ConstraintSummaryWithObervations](_.constraintGroups)
    val observations     = Focus[ConstraintSummaryWithObervations](_.observations)

  private def obsResultToSummary(obsR: ObservationResult): ObsSummaryWithTitleAndConf =
    ObsSummaryWithTitleAndConf(
      obsR.id,
      obsR.title,
      obsR.subtitle,
      obsR.status,
      obsR.activeStatus,
      obsR.plannedTime.execution,
      obsR.scienceMode
    )

  private val queryToConstraintsWithObsGetter
    : Getter[ConstraintGroupObsQuery.Data, ConstraintSummaryWithObervations] =
    data =>
      ConstraintSummaryWithObervations(
        data.constraintSetGroup.matches
          .toSortedMap(ConstraintGroup.obsIds.get),
        data.observations.matches
          .map(obsResultToSummary)
          .toSortedMap(ObsSummaryWithTitleAndConf.id.get)
      )

  extension (self: ConstraintGroupObsQuery.Data.type)
    def asConstraintSummWithObs = queryToConstraintsWithObsGetter
