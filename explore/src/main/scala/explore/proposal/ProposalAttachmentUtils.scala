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
import explore.model.Attachment
import explore.model.AttachmentList
import explore.model.syntax.all.*
import explore.syntax.ui.*
import explore.utils.OdbRestClient
import explore.utils.ToastCtx
import fs2.dom
import japgolly.scalajs.react.*
import lucuma.core.enums.AttachmentType
import lucuma.core.model.Program
import lucuma.core.util.Timestamp
import org.scalajs.dom.File as DomFile
import org.typelevel.log4cats.Logger

trait ProposalAttachmentUtils extends AttachmentUtils:
  def insertAttachment(
    programId:      Program.Id,
    modAttachments: Endo[AttachmentList] => Callback,
    client:         OdbRestClient[IO],
    attType:        AttachmentType,
    files:          List[DomFile]
  )(using ToastCtx[IO]): IO[Unit] =
    files.headOption
      .map(f =>
        checkFileSize(f) {
          val name = NonEmptyString.unsafeFrom(f.name)
          client
            .insertAttachment(
              programId,
              attType,
              name,
              none,
              dom.readReadableStream(IO(f.stream()))
            )
            .toastErrors
            .flatMap(id =>
              IO.now().flatMap { now =>
                modAttachments(
                  _.updated(id,
                            Attachment(
                              id,
                              attType,
                              name,
                              none,
                              false,
                              f.size.toLong,
                              Timestamp.unsafeFromInstantTruncated(now)
                            )
                  )
                ).toAsync
              }
            )
        }
      )
      .orEmpty

  def updateAttachment(
    programId:      Program.Id,
    modAttachments: Endo[AttachmentList] => Callback,
    client:         OdbRestClient[IO],
    att:            Attachment,
    files:          List[DomFile]
  )(using
    ToastCtx[IO]
  ): IO[Unit] =
    files.headOption
      .map(f =>
        checkFileSize(f) {
          val name = NonEmptyString.unsafeFrom(f.name)
          client
            .updateAttachment(
              programId,
              att.id,
              name,
              none,
              dom.readReadableStream(IO(f.stream()))
            ) *>
            IO.now()
              .flatMap { now =>
                modAttachments(
                  _.updatedWith(att.id)(
                    _.map(
                      _.copy(fileName = name, updatedAt = Timestamp.unsafeFromInstantTruncated(now))
                    )
                  )
                ).toAsync
              }
              .toastErrors
        }
      )
      .orEmpty

  def deleteAttachment(
    programId:      Program.Id,
    modAttachments: Endo[AttachmentList] => Callback,
    client:         OdbRestClient[IO],
    attId:          Attachment.Id
  )(using ToastCtx[IO]): IO[Unit] =
    modAttachments(_.removed(attId)).toAsync *>
      client.deleteAttachment(programId, attId).toastErrors

  def getAttachmentUrl(
    programId: Program.Id,
    attId:     Attachment.Id,
    client:    OdbRestClient[IO]
  ): IO[Pot[String]] =
    client.getAttachmentUrl(programId, attId).attempt.map {
      case Right(url) => Pot(url)
      case Left(t)    => Pot.error(t)
    }

  def onInsertFileSelected(
    programId:      Program.Id,
    modAttachments: Endo[AttachmentList] => Callback,
    attType:        AttachmentType,
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
    modAttachments: Endo[AttachmentList] => Callback,
    thisAtt:        Attachment,
    client:         OdbRestClient[IO],
    action:         View[Action]
  )(e: ReactEventFromInput)(using ToastCtx[IO], Logger[IO]): Callback =
    val files = e.target.files.toList
    (Callback(e.target.value = null) *>
      updateAttachment(programId, modAttachments, client, thisAtt, files)
        .switching(action.async, Action.Replace, Action.None)
        .runAsync)
      .when_(files.nonEmpty)
