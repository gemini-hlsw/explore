// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB
// gql: import explore.model.decoders._
// gql: import io.circe.refined._
// gql: import lucuma.ui.reusability._

object TargetQueriesGQL {

  @GraphQL
  trait TargetEditQuery extends GraphQLOperation[ObservationDB] {
    val document = """
      query($id: TargetId!) {
        target(targetId: $id) {
          id
          name
          tracking {
            ... on Sidereal {
              coordinates {
                ra {
                  microarcseconds
                }
                dec {
                  microarcseconds
                }
              }
              epoch
              properMotion {
                ra {
                  microarcsecondsPerYear
                }
              	dec {
                  microarcsecondsPerYear
                }
							}
              radialVelocity {
                centimetersPerSecond
              }
              parallax {
                microarcseconds
              }
            }
          }
          magnitudes {
            value
            band
            system
          }
        }
      }
      """

    object Data {
      object Target {
        type Tracking   = lucuma.core.model.SiderealTracking
        type Magnitudes = lucuma.core.model.Magnitude
      }
    }
  }

  @GraphQL
  trait TargetEditSubscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription($id: TargetId!) {
        targetEdit(targetId: $id) {
          id
        }
      }
      """
  }

  @GraphQL
  trait TargetMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($input: EditSiderealInput!) {
        updateSiderealTarget(input: $input) {
          id
        }
      }
    """
  }
}
