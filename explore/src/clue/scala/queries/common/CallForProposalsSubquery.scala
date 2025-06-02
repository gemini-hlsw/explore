// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import explore.model.CallForProposal
import lucuma.schemas.ObservationDB

@GraphQL
object CallForProposalsSubquery
    extends GraphQLSubquery.Typed[ObservationDB, CallForProposal]("CallForProposal"):
  override val subquery: String = s"""
    {
      id
      semester
      title
      cfpType: type
      nonPartnerDeadline
      active {
        start
        end
      }
      partners {
        partner
        submissionDeadline
      }
      allowsNonPartnerPi
      instruments
      coordinateLimits {
        north $SiteCoordinatesLimitsSubquery
        south $SiteCoordinatesLimitsSubquery
      }
    }
  """
