// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import explore.model.Attachment
import lucuma.schemas.ObservationDB

@GraphQL
object AttachmentSubquery extends GraphQLSubquery.Typed[ObservationDB, Attachment]("Attachment"):
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
