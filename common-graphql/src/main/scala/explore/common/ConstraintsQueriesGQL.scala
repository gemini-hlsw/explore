// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.model.ConstraintSetModel
import explore.schemas._
// gql: import explore.model.reusability._
// gql: import lucuma.ui.reusability._

object ConstraintsQueriesGQL {

  @GraphQL
  trait ConstraintSetQuery extends GraphQLOperation[ObservationDB] {
    val document = """
      query($id: ConstraintSetId!) {
        constraintSet(constraintSetId: $id) {
          id
          name
          cloudExtinction
          imageQuality
          skyBackground
          waterVapor
          elevationRange {
            type: __typename
            ... on AirMassRange {
              min
              max
            }
            ... on HourAngleRange {
              minHours
              maxHours
            }
          }
        }
      }
      """

    object Data {
      type ConstraintSet = ConstraintSetModel
    }
  }

  @GraphQL
  trait ConstraintSetEditSubscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription($id: ConstraintSetId!) {
        constraintSetEdit(constraintSetId: $id) {
          id
        }
      }
    """
  }

  @GraphQL
  trait Mutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation ($input: EditConstraintSetInput!){
        updateConstraintSet(input: $input) {
          id
        }
      }
    """
  }

}
