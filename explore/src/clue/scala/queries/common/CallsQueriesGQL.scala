// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB

object CallsQueriesGQL:
  @GraphQL
  trait ReadOpenCFPs extends GraphQLOperation[ObservationDB]:
    val document: String = s"""
      query {
        callsForProposals(WHERE: {isOpen: {EQ: true}}) {
          matches {
            id
            semester
            title
            cfpType: type
            nonPartnerDeadline
            partners {
              partner
              submissionDeadline
            }
          }
        }
      }
    """

    object Data:
      object CallsForProposals:
        type Matches = explore.model.CallForProposal
