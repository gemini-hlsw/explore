// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.Order.*
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.generic.semiauto.*
import io.circe.refined.given
import lucuma.core.model
import lucuma.core.model.Observation
import lucuma.core.util.Timestamp
import lucuma.schemas.ObservationDB.Enums.ObsAttachmentType
import monocle.Focus
import monocle.Lens

import scala.collection.immutable.SortedSet

case class ObsAttachment(
  id:             model.ObsAttachment.Id,
  attachmentType: ObsAttachmentType,
  fileName:       NonEmptyString,
  description:    Option[NonEmptyString],
  checked:        Boolean,
  fileSize:       Long,
  updatedAt:      Timestamp,
  observations:   SortedSet[Observation.Id]
) derives Eq

object ObsAttachment:
  val id: Lens[ObsAttachment, model.ObsAttachment.Id]              = Focus[ObsAttachment](_.id)
  val attachmentType: Lens[ObsAttachment, ObsAttachmentType]       =
    Focus[ObsAttachment](_.attachmentType)
  val fileName: Lens[ObsAttachment, NonEmptyString]                = Focus[ObsAttachment](_.fileName)
  val description: Lens[ObsAttachment, Option[NonEmptyString]]     = Focus[ObsAttachment](_.description)
  val checked: Lens[ObsAttachment, Boolean]                        = Focus[ObsAttachment](_.checked)
  val fileSize: Lens[ObsAttachment, Long]                          = Focus[ObsAttachment](_.fileSize)
  val updatedAt: Lens[ObsAttachment, Timestamp]                    = Focus[ObsAttachment](_.updatedAt)
  val observations: Lens[ObsAttachment, SortedSet[Observation.Id]] =
    Focus[ObsAttachment](_.observations)

  private case class ObsIdMatch(id: Observation.Id)
  private given Decoder[ObsIdMatch] = deriveDecoder

  private case class ObsIdMatches(matches: List[ObsIdMatch])
  private given Decoder[ObsIdMatches] = deriveDecoder

  given Decoder[ObsAttachment] = Decoder.instance(c =>
    for {
      id   <- c.get[model.ObsAttachment.Id]("id")
      tpe  <- c.get[ObsAttachmentType]("attachmentType")
      name <- c.get[NonEmptyString]("fileName")
      desc <- c.get[Option[NonEmptyString]]("description")
      chkd <- c.get[Boolean]("checked")
      size <- c.get[Long]("fileSize")
      updt <- c.get[Timestamp]("updatedAt")
      oids <- c.downField("observations").as[ObsIdMatches].map(_.matches.map(_.id))
    } yield ObsAttachment(id, tpe, name, desc, chkd, size, updt, SortedSet.from(oids))
  )
