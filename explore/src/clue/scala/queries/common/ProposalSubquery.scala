// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import explore.model.Proposal
import explore.model.PartnerSplit
import lucuma.schemas.ObservationDB

@GraphQL
object ProposalSubquery extends GraphQLSubquery.Typed[ObservationDB, Proposal]("Proposal"):
  override val subquery: String = s"""
    {
      call $CallForProposalsSubquery
      category
      reference {
        label
      }
      type {
        scienceSubtype
        ... on Classical {
          minPercentTime
          partnerSplits $PartnerSplitSubquery
        }
        ... on DirectorsTime {
          toOActivation
          minPercentTime
        }
        ... on FastTurnaround {
          toOActivation
          minPercentTime
          piAffiliation
        }
        ... on LargeProgram {
          toOActivation
          minPercentTime
          minPercentTotalTime
          totalTime {
            hours
            minutes
          }
        }
        ... on Queue {
          toOActivation
          minPercentTime
          partnerSplits $PartnerSplitSubquery
        }
        ... on SystemVerification {
          toOActivation
          minPercentTime
        }
      }
    }
  """

@GraphQL
object PartnerSplitSubquery
    extends GraphQLSubquery.Typed[ObservationDB, PartnerSplit]("PartnerSplit"):
  override val subquery: String = """
    {
      partner
      percent
    }
  """
