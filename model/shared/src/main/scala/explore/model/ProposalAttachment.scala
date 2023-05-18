// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.generic.semiauto.*
import io.circe.refined.given
import lucuma.core.util.Timestamp
import lucuma.schemas.ObservationDB.Enums.ProposalAttachmentType
import monocle.Focus
import monocle.Lens

case class ProposalAttachment(
  attachmentType: ProposalAttachmentType,
  fileName:       NonEmptyString,
  description:    Option[NonEmptyString],
  checked:        Boolean,
  fileSize:       Long,
  updatedAt:      Timestamp
) derives Eq

object ProposalAttachment:
  val attachmentType: Lens[ProposalAttachment, ProposalAttachmentType] =
    Focus[ProposalAttachment](_.attachmentType)
  val fileName: Lens[ProposalAttachment, NonEmptyString]               = Focus[ProposalAttachment](_.fileName)
  val description: Lens[ProposalAttachment, Option[NonEmptyString]]    =
    Focus[ProposalAttachment](_.description)
  val checked: Lens[ProposalAttachment, Boolean]                       = Focus[ProposalAttachment](_.checked)
  val fileSize: Lens[ProposalAttachment, Long]                         = Focus[ProposalAttachment](_.fileSize)
  val updatedAt: Lens[ProposalAttachment, Timestamp]                   = Focus[ProposalAttachment](_.updatedAt)

  given Decoder[ProposalAttachment] = deriveDecoder
