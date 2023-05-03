// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.model
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.*
import io.circe.Decoder
import io.circe.generic.semiauto

object EnumQueriesGQL:
  @GraphQL
  trait ObsAttachmentTypeMetaQuery extends GraphQLOperation[ObservationDB]:
    val document: String = s"""
      query {
        obsAttachmentTypeMeta {
          tag
          shortName
          longName
        }
      }
    """

    given Decoder[explore.model.enums.ObsAttachmentType] =
      semiauto.deriveDecoder

    object Data:
      type ObsAttachmentTypeMeta = explore.model.enums.ObsAttachmentType
