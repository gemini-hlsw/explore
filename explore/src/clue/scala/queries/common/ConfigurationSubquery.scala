// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import lucuma.core.model.Configuration
import lucuma.odb.json.configurationrequest.query.given
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.DecSubquery
import lucuma.schemas.odb.RASubquery

@GraphQL
object ConfigurationSubquery
    extends GraphQLSubquery.Typed[ObservationDB, Configuration]("Configuration"):
  override val subquery: String = s"""
    {
      conditions {
        imageQuality
        cloudExtinction
        skyBackground
        waterVapor
      }
      referenceCoordinates {
        ra $RASubquery
        dec $DecSubquery
      }
      observingMode {
        instrument
        mode
        gmosNorthLongSlit {
          grating
        }
        gmosSouthLongSlit {
          grating
        }
      }
    }
  """
