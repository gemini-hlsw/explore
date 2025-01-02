// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.*
// gql: import lucuma.odb.json.configurationrequest.query.given
// gql: import lucuma.schemas.decoders.given

object ProgramSummaryQueriesGQL {
  @GraphQL
  trait AllProgramObservations extends GraphQLOperation[ObservationDB] {
    val document: String = s"""
      query($$where: WhereObservation!, $$OFFSET: ObservationId) {
        observations(WHERE: $$where, OFFSET: $$OFFSET) {
          matches $ObservationSubquery
          hasMore
        }
      }
    """
  }

  @GraphQL
  trait AllProgramTargets extends GraphQLOperation[ObservationDB] {
    val document: String = s"""
      query($$where: WhereTarget!, $$OFFSET: TargetId) {
        targets(WHERE: $$where, OFFSET: $$OFFSET) {
          matches $TargetWithIdSubquery
          hasMore
        }
      }
    """
  }

  @GraphQL
  trait AllProgramConfigurationRequests extends GraphQLOperation[ObservationDB] {
    val document: String = s"""
      query($$pid: ProgramId!, $$OFFSET: ConfigurationRequestId) {
        program(programId: $$pid) {
          configurationRequests(OFFSET: $$OFFSET) {
            matches $ConfigurationRequestSubquery
            hasMore
          }
        }
      }
    """
  }

  @GraphQL
  trait AllProgramAttachments extends GraphQLOperation[ObservationDB] {
    val document: String = s"""
      query($$programId: ProgramId!) {
        program(programId: $$programId) {
          attachments $AttachmentSubquery
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

  @GraphQL
  trait ProgramTimesQuery extends GraphQLOperation[ObservationDB] {
    val document: String = s"""
      query($$programId: ProgramId!) {
        program(programId: $$programId) $ProgramTimesSubquery
      }
    """
  }

  @GraphQL
  trait ObservationsWorkflowQuery extends GraphQLOperation[ObservationDB] {
    val document: String = s"""
      query($$where: WhereObservation!) {
        observations(WHERE: $$where) {
          matches {
            id
            workflow $ObservationWorkflowSubquery
          }
        }
      }
    """
  }

  @GraphQL
  trait ObservationExecutionQuery extends GraphQLOperation[ObservationDB] {
    val document: String = s"""
      query($$id: ObservationId!) {
        observation(observationId: $$id) {
          execution $ExecutionSubquery
        }
      }
    """
  }

  @GraphQL
  trait GroupTimeRangeQuery extends GraphQLOperation[ObservationDB] {
    val document: String = s"""
      query($$groupId: GroupId!) {
        group(groupId: $$groupId) {
          timeEstimateRange $ProgramTimeRangeSubquery
        }
      }
    """
  }
}
