// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import explore.model.ObsSummary
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.*
import clue.annotation.GraphQL

@GraphQL
object ObservationSummarySubquery
    extends GraphQLSubquery.Typed[ObservationDB, ObsSummary]("Observation"):
  override val subquery: String = s"""
        {
          id
          title
          subtitle
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
          constraintSet $ConstraintSetSubquery
          scienceRequirements {
            spectroscopy {
              wavelength $WavelengthSubquery
            }
          }
          observingMode $BasicConfigurationSubquery
        }
      """
