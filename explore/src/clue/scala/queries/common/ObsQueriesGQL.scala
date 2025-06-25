// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.*
// gql: import lucuma.odb.json.configurationrequest.query.given
// gql: import lucuma.odb.json.sequence.given
// gql: import lucuma.schemas.decoders.given

object ObsQueriesGQL:
  @GraphQL
  trait ProgramCreateObservation extends GraphQLOperation[ObservationDB]:
    val document = s"""
      mutation($$createObservation: CreateObservationInput!) {
        createObservation(input: $$createObservation) {
          observation $ObservationSubquery
        }
      }
    """

  @GraphQL
  trait SequenceOffsets extends GraphQLOperation[ObservationDB]:
    val document = s"""
      query($$obsId: ObservationId!) {
        observation(observationId: $$obsId) {
          execution {
            digest {
              acquisition {
                offsets $OffsetSubquery
              }
              science {
                offsets $OffsetSubquery
              }
            }
          }
        }
      }
    """

    object Data:
      object Observation:
        type Execution = explore.model.ExecutionOffsets

  @GraphQL
  trait ObservationEditSubscription extends GraphQLOperation[ObservationDB]:
    // We need to include the `value {id}` to avoid a bug in grackle.
    val document = """
      subscription($input: ObservationEditInput!) {
        observationEdit(input: $input) {
          observationId
        }
      }
    """

  @GraphQL
  trait UpdateObservationMutation extends GraphQLOperation[ObservationDB]:
    val document = """
      mutation ($input: UpdateObservationsInput!){
        updateObservations(input: $input) {
          observations { id }
        }
      }
    """

  @GraphQL
  trait UpdateObservationTimesMutation extends GraphQLOperation[ObservationDB]:
    val document = """
      mutation ($input: UpdateObservationsTimesInput!){
        updateObservationsTimes(input: $input) {
          observations { id }
        }
      }
    """

  @GraphQL
  trait UpdateConfigurationMutation extends GraphQLOperation[ObservationDB]:
    val document = s"""
      mutation ($$input: UpdateObservationsInput!){
        updateObservations(input: $$input) {
          observations {
            observingMode $ObservingModeSubquery
          }
        }
      }
    """

  @GraphQL
  trait SetObservationWorkflowStateMutation extends GraphQLOperation[ObservationDB]:
    val document = s"""
      mutation ($$input: SetObservationWorkflowStateInput!){
        setObservationWorkflowState(input: $$input) {
          state
        }
      }
    """

  @GraphQL
  trait CloneObservationMutation extends GraphQLOperation[ObservationDB]:
    val document = s"""
      mutation ($$input: CloneObservationInput!){
        cloneObservation(input: $$input) {
          newObservation $ObservationSubquery
        }
      }
    """

  @GraphQL
  trait CreateConfigurationRequestMutation extends GraphQLOperation[ObservationDB]:
    val document = s"""
      mutation($$input: CreateConfigurationRequestInput!) {
        createConfigurationRequest(input: $$input)
          $ConfigurationRequestSubquery
      }
    """

  @GraphQL
  trait ProgramObservationsDelta extends GraphQLOperation[ObservationDB]:
    val document = s"""
      subscription($$input: ObservationEditInput!) {
        observationEdit(input: $$input) {
          observationId
          value $ObservationSubquery
          meta:value { existence }
          editType
        }
      }
    """

  @GraphQL
  trait ObsCalcSubscription extends GraphQLOperation[ObservationDB]:
    val document = s"""
      subscription($$input: ObscalcUpdateInput!) {
        obscalcUpdate(input: $$input) {
          observationId
          oldState
          newState
          meta:value { existence }
          editType
          value {
            groupId
            execution {
              calculatedDigest $CalculatedDigestSubquery
            }
            calculatedWorkflow $CalculatedObservationWorkflowSubquery
          }
        }
      }
    """

  @GraphQL
  trait ResolveObsReference extends GraphQLOperation[ObservationDB]:
    val document = """
      query($input: ObservationReferenceLabel) {
        observation(observationReference: $input) {
          id
          program { id }
        }
      }
    """
