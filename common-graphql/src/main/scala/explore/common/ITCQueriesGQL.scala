// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.schemas.ITC
// import explore.schemas.itct.SpectroscopyModeInput
// gql: import lucuma.ui.reusability._
// gql: import explore.model.decoders._
// gql: import explore.model.reusability._
// gql: import io.circe.refined._

object ITCQueriesGQL {

  @GraphQL
  trait SpectroscopyITCQuery extends GraphQLOperation[ITC] {
    val document = """
      query($input: SpectroscopyModeInput) {
        spectroscopy(input: $input) {
          results {
            mode {
              instrument
            }
          }
        }
      }
    """
    // case class Variables(input: SpectroscopyModeInput)

    object Data {
      // object Observations {
      //   object Nodes {
      //   //   trait ConstraintSet extends ConstraintsSummary
      //   //   object PlannedTime {
      //   //     type Execution = time.Duration
      //   //   }
      //   // }
      // }
      //
      // object ConstraintSetGroup {
      //   object Nodes {
      //     type ConstraintSet = model.ConstraintSet
      //   }
      // }
    }

  }

}
