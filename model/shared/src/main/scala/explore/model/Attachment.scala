// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.*
import io.circe.generic.semiauto
import io.circe.generic.semiauto.*
import io.circe.refined.given
import lucuma.core.enums.AttachmentPurpose
import lucuma.core.enums.AttachmentType
import lucuma.core.model
import lucuma.core.util.Timestamp
import monocle.Focus
import monocle.Lens

case class Attachment(
  id:             Attachment.Id,
  attachmentType: AttachmentType,
  fileName:       NonEmptyString,
  description:    Option[NonEmptyString],
  checked:        Boolean,
  fileSize:       Long,
  updatedAt:      Timestamp
) derives Eq:
  def isForPurpose(purpose: AttachmentPurpose): Boolean        =
    attachmentType.purpose === purpose
  def isForPurposes(purposes: Set[AttachmentPurpose]): Boolean =
    purposes.contains(attachmentType.purpose)

object Attachment:
  type Id = model.Attachment.Id
  val Id = model.Attachment.Id

  val id: Lens[Attachment, Id]                              = Focus[Attachment](_.id)
  val attachmentType: Lens[Attachment, AttachmentType]      = Focus[Attachment](_.attachmentType)
  val fileName: Lens[Attachment, NonEmptyString]            = Focus[Attachment](_.fileName)
  val description: Lens[Attachment, Option[NonEmptyString]] = Focus[Attachment](_.description)
  val checked: Lens[Attachment, Boolean]                    = Focus[Attachment](_.checked)
  val fileSize: Lens[Attachment, Long]                      = Focus[Attachment](_.fileSize)
  val updatedAt: Lens[Attachment, Timestamp]                = Focus[Attachment](_.updatedAt)

  given Decoder[Attachment] = deriveDecoder
