// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.model.sequence.SequenceDigest
import lucuma.core.util.CalculatedValue
import lucuma.odb.json.sequence.given
import lucuma.schemas.ObservationDB
import lucuma.schemas.decoders.given
import lucuma.schemas.odb.OffsetSubquery
import lucuma.schemas.odb.TimeSpanSubquery

@GraphQL
object CalculatedDigestSubquery
    extends GraphQLSubquery.Typed[ObservationDB, CalculatedValue[Option[ExecutionDigest]]](
      "CalculatedExecutionDigest"
    ):
  override val subquery: String = s"""
    {
      state
      value {
        setup {
          full $TimeSpanSubquery
          reacquisition $TimeSpanSubquery
        }
        acquisition $SequenceDigestSubquery
        science $SequenceDigestSubquery
      }
    }
  """

@GraphQL
object SequenceDigestSubquery
    extends GraphQLSubquery.Typed[ObservationDB, SequenceDigest]("SequenceDigest"):
  override val subquery: String = s"""
      {
        observeClass
        atomCount
        timeEstimate {
          program $TimeSpanSubquery
          nonCharged $TimeSpanSubquery
          total $TimeSpanSubquery
        }
        offsets $OffsetSubquery
        executionState
      }
  """
