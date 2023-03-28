// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.annotation.*
import clue.GraphQLSubquery
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.*
import explore.model.ObsSummaryWithConstraintsAndConf

@GraphQL
object ObservationSubquery
    extends GraphQLSubquery.Typed[
      ObservationDB,
      AsterismQueriesGQL.AsterismGroupObsQuery.Data.Observations.Matches
    ](
      "Observation"
    ):
  override val subquery: String = s"""
        {
          id
          constraintSet $ConstraintsSummarySubquery
          status
          activeStatus
          visualizationTime
          posAngleConstraint $PosAngleConstraintSubquery
          plannedTime {
            execution $TimeSpanSubquery
          }
          targetEnvironment {
            asterism {
              id
            }
          }
          scienceRequirements {
            spectroscopy {
              wavelength $WavelengthSubquery
            }
          }
          observingMode $BasicConfigurationSubquery
        }
      """
