// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.*
// gql: import lucuma.schemas.decoders.given

object ProgramSummaryQueriesGQL {
  @GraphQL
  trait AllProgramObservations extends GraphQLOperation[ObservationDB] {
    val document: String = s"""
      query($$programId: ProgramId!, $$OFFSET: ObservationId) {
        observations(programId: $$programId, OFFSET: $$OFFSET) {
          matches $ObservationSummarySubquery
          hasMore
        }
      }
    """
  }
  @GraphQL
  trait AllProgramTargets      extends GraphQLOperation[ObservationDB] {
    val document: String = s"""
      query($$programId: ProgramId!, $$OFFSET: TargetId) {
        targets(WHERE: { programId: { EQ: $$programId } }, OFFSET: $$OFFSET) {
          matches $TargetWithIdSubquery
          hasMore
        }
      }
    """
  }

  @GraphQL
  trait AllProgramAttachments extends GraphQLOperation[ObservationDB] {
    val document: String = s"""
      query($$programId: ProgramId!) {
        program(programId: $$programId) {
          obsAttachments $ObsAttachmentSubquery
          proposalAttachments $ProposalAttachmentSubquery
        }
      }
    """
  }

  @GraphQL
  trait AllPrograms extends GraphQLOperation[ObservationDB] {
    val document: String = s"""
      query($$OFFSET: ProgramId) {
        programs(OFFSET: $$OFFSET, includeDeleted: true) {
          matches $ProgramInfoSubquery
          hasMore
        }
      }
    """
  }

  @GraphQL
  trait ProgramDetailsQuery extends GraphQLOperation[ObservationDB] {
    val document: String = s"""
      query($$programId: ProgramId!) {
        program(programId: $$programId) $ProgramDetailsSubquery
      }
    """
  }
}
