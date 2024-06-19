// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.Endo
import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.react.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.attachments.Action
import explore.attachments.AttachmentUtils
import explore.model.ProposalAttachment
import explore.model.syntax.all.*
import explore.syntax.ui.*
import explore.utils.OdbRestClient
import explore.utils.ToastCtx
import fs2.dom
import japgolly.scalajs.react.*
import lucuma.core.model.Program
import lucuma.core.util.Timestamp
import lucuma.schemas.enums.ProposalAttachmentType
import org.scalajs.dom.File as DomFile
import org.typelevel.log4cats.Logger

trait ProposalAttachmentUtils extends AttachmentUtils:
  def insertAttachment(
    programId:      Program.Id,
    modAttachments: Endo[List[ProposalAttachment]] => Callback,
    client:         OdbRestClient[IO],
    attType:        ProposalAttachmentType,
    files:          List[DomFile]
  )(using ToastCtx[IO]): IO[Unit] =
    files.headOption
      .map(f =>
        checkFileSize(f) {
          val name = NonEmptyString.unsafeFrom(f.name)
          client
            .insertProposalAttachment(
              programId,
              attType,
              name,
              dom.readReadableStream(IO(f.stream()))
            )
            .toastErrors
            .flatMap(_ =>
              IO.now().flatMap { now =>
                modAttachments(l =>
                  ProposalAttachment(
                    attType,
                    name,
                    f.size.toLong,
                    Timestamp.unsafeFromInstantTruncated(now)
                  ) :: l
                ).toAsync
              }
            )
        }
      )
      .orEmpty

  def updateAttachment(
    programId:      Program.Id,
    modAttachments: Endo[List[ProposalAttachment]] => Callback,
    client:         OdbRestClient[IO],
    att:            ProposalAttachment,
    files:          List[DomFile]
  )(using
    ToastCtx[IO]
  ): IO[Unit] =
    files.headOption
      .map(f =>
        checkFileSize(f) {
          val name = NonEmptyString.unsafeFrom(f.name)
          client
            .updateProposalAttachment(
              programId,
              att.attachmentType,
              name,
              dom.readReadableStream(IO(f.stream()))
            ) *>
            IO.now()
              .flatMap { now =>
                modAttachments(
                  _.map(pa =>
                    if (pa.attachmentType === att.attachmentType)
                      pa.copy(
                        fileName = name,
                        updatedAt = Timestamp.unsafeFromInstantTruncated(now)
                      )
                    else pa
                  )
                ).toAsync
              }
              .toastErrors
        }
      )
      .orEmpty

  def deleteAttachment(
    programId:      Program.Id,
    modAttachments: Endo[List[ProposalAttachment]] => Callback,
    client:         OdbRestClient[IO],
    attType:        ProposalAttachmentType
  )(using ToastCtx[IO]): IO[Unit] =
    modAttachments(_.filterNot(_.attachmentType === attType)).toAsync *>
      client.deleteProposalAttachment(programId, attType).toastErrors

  def getAttachmentUrl(
    programId: Program.Id,
    attType:   ProposalAttachmentType,
    client:    OdbRestClient[IO]
  ): IO[Pot[String]] =
    client.getProposalAttachmentUrl(programId, attType).attempt.map {
      case Right(url) => Pot(url)
      case Left(t)    => Pot.error(t)
    }

  def onInsertFileSelected(
    programId:      Program.Id,
    modAttachments: Endo[List[ProposalAttachment]] => Callback,
    attType:        ProposalAttachmentType,
    client:         OdbRestClient[IO],
    action:         View[Action]
  )(e: ReactEventFromInput)(using ToastCtx[IO], Logger[IO]): Callback =
    val files = e.target.files.toList
    (Callback(e.target.value = null) *>
      insertAttachment(programId, modAttachments, client, attType, files)
        .switching(action.async, Action.Insert, Action.None)
        .runAsync).when_(files.nonEmpty)

  def onUpdateFileSelected(
    programId:      Program.Id,
    modAttachments: Endo[List[ProposalAttachment]] => Callback,
    thisAtt:        ProposalAttachment,
    client:         OdbRestClient[IO],
    action:         View[Action]
  )(e: ReactEventFromInput)(using ToastCtx[IO], Logger[IO]): Callback =
    val files = e.target.files.toList
    (Callback(e.target.value = null) *>
      updateAttachment(programId, modAttachments, client, thisAtt, files)
        .switching(action.async, Action.Replace, Action.None)
        .runAsync)
      .when_(files.nonEmpty)
