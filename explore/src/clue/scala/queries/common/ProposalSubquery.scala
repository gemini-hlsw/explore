// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import explore.model.Proposal
import lucuma.schemas.ObservationDB

@GraphQL
object ProposalSubquery extends GraphQLSubquery.Typed[ObservationDB, Proposal]("Proposal"):
  override val subquery: String = """
    {
      call {
        id
      }
      title
      abstract
      category
    }
  """

  // proposalClass {
  //   __typename
  //   minPercentTime
  //   ... on LargeProgram {
  //     minPercentTotalTime
  //     totalTime {
  //       microseconds
  //     }
  //   }
  //   ... on Intensive {
  //     minPercentTotalTime
  //     totalTime {
  //       microseconds
  //     }
  //   }
  // }
  // category
  // toOActivation
  // abstract
  // partnerSplits {
  //   partner
  //   percent
  // }
