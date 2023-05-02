// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import explore.model.ObsAttachment
import lucuma.schemas.ObservationDB
import clue.annotation.GraphQL
import explore.DynamicEnums.given

@GraphQL
object ObsAttachmentSubquery
    extends GraphQLSubquery.Typed[ObservationDB, ObsAttachment]("ObsAttachment"):
  override val subquery: String = s"""
    {
      id
      attachmentType
      fileName
      description
      checked
      fileSize
      updatedAt
    }
  """
