// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import explore.model.ObsAttachment
import lucuma.schemas.ObservationDB

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
