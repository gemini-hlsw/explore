// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB
// gql: import explore.model.reusability._
// gql: import lucuma.schemas.decoders._
// gql: import lucuma.ui.reusability._

object TargetQueriesGQL {

  @GraphQL
  trait TargetNameQuery extends GraphQLOperation[ObservationDB] {
    // FIXME Change this to an actual name patter query when it's available in the API
    val document = """
      query {
        scienceTargetGroup(programId: "p-2") {
          commonTarget {
            name
            tracking {
              ... on Sidereal {
                catalogId {
                  name
                  id
                }
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
      }
    """

    object Data {
      object ScienceTargetGroup {
        type CommonTarget = lucuma.core.model.Target
      }
    }
  }

  @GraphQL
  trait SiderealTargetMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($input: EditSiderealInput!) {
        updateScienceTarget(input: { editSidereal: $input} ) {
          edits {
            target {
              id
            }
          }
        }
      }
    """
  }
}
