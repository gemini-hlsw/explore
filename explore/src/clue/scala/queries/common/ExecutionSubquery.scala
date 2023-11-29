package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import explore.model.Execution
import lucuma.core.model.sequence.SequenceDigest
import lucuma.odb.json.sequence.given
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.OffsetSubquery

@GraphQL
object ExecutionSubquery extends GraphQLSubquery.Typed[ObservationDB, Execution]("Execution") {
  override val subquery: String = s"""#gql
    {
      digest {
        setup {
          full {
            microseconds
          }
          reacquisition {
            microseconds
          }
        }
        acquisition $SequenceDigestSubquery
        science $SequenceDigestSubquery
      }
    }
  """
}

@GraphQL
object SequenceDigestSubquery
    extends GraphQLSubquery.Typed[ObservationDB, SequenceDigest]("SequenceDigest") {
  override val subquery: String = s"""
      {
        observeClass
        atomCount
        plannedTime {
          charges {
            chargeClass
            time {
              microseconds
            }
          }
          total {
            microseconds
          }
        }
        offsets $OffsetSubquery
      }
  """
}
