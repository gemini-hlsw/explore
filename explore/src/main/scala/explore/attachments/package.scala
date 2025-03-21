// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.attachments

import cats.Eq
import cats.derived.*
import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.react.*
import eu.timepit.refined.types.numeric.NonNegLong
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.Attachment
import explore.model.AttachmentList
import explore.model.syntax.all.*
import explore.syntax.ui.*
import explore.utils.*
import fs2.dom
import japgolly.scalajs.react.*
import lucuma.core.enums.AttachmentType
import lucuma.core.model.Program
import lucuma.core.util.Timestamp
import lucuma.react.primereact.Message
import lucuma.react.primereact.PrimeStyles
import lucuma.react.table.ColumnId
import lucuma.refined.*
import lucuma.ui.primereact.LucumaPrimeStyles
import org.scalajs.dom.File as DomFile
import org.typelevel.log4cats.Logger

enum Action(val msg: String) derives Eq:
  case None     extends Action("")
  case Insert   extends Action("Uploading Attachment")
  case Replace  extends Action("Uploading Replacment")
  case Download extends Action("Downloading Attachment")
  case Unlink   extends Action("Unlinking Attachment")

object Action:
  given Reusability[Action] = Reusability.byEq

trait AttachmentUtils:
  // TODO: Maybe we can have a graphql query for getting information such as this? This is a config var in ODB.
  private val maxFileSize: NonNegLong = 10000000.refined

  def checkFileSize(file: DomFile)(f: => IO[Unit])(using tx: ToastCtx[IO]): IO[Unit] =
    if (file.size.toLong === 0)
      tx.showToast("Attachment files cannot be empty", Message.Severity.Error, true)
    else if (file.size.toLong > maxFileSize.value)
      tx.showToast(
        s"Attachment files cannot be larger than ${maxFileSize.toHumanReadableByteCount}",
        Message.Severity.Error,
        true
      )
    else f

  val AttIdColumnId: ColumnId          = ColumnId("id")          // ObsAttachments only
  val ActionsColumnId: ColumnId        = ColumnId("actions")
  val FileNameColumnId: ColumnId       = ColumnId("filename")
  val AttachmentTypeColumnId: ColumnId = ColumnId("attachment-type")
  val SizeColumnId                     = ColumnId("filesize")
  val LastUpdateColumnId               = ColumnId("last-update")
  val AssignmentsColumnId: ColumnId    = ColumnId("assignments") // ObsAttachments only
  val DescriptionColumnId: ColumnId    = ColumnId("description")
  val CheckedColumnId: ColumnId        = ColumnId("checked")

  val columnNames: Map[ColumnId, String] = Map(
    AttIdColumnId          -> "ID",
    ActionsColumnId        -> "Actions",
    FileNameColumnId       -> "File",
    AttachmentTypeColumnId -> "Type",
    SizeColumnId           -> "Size",
    LastUpdateColumnId     -> "LastUpdate",
    AssignmentsColumnId    -> "Assignments",
    DescriptionColumnId    -> "Description",
    CheckedColumnId        -> "Checked"
  )

  val LabelButtonClasses =
    PrimeStyles.Component |+| PrimeStyles.Button |+| PrimeStyles.ButtonIconOnly
      |+| LucumaPrimeStyles.Tiny |+| LucumaPrimeStyles.Compact

  val tableLabelButtonClasses = LabelButtonClasses |+| PrimeStyles.ButtonSecondary

trait ObsAttachmentUtils extends AttachmentUtils:
  type UrlMapKey = (Attachment.Id, Timestamp)
  type UrlMap    = Map[UrlMapKey, Pot[String]]

  extension (oa: Attachment) def toMapKey: UrlMapKey = (oa.id, oa.updatedAt)

  def insertAttachment(
    programId:   Program.Id,
    attachments: View[AttachmentList],
    client:      OdbRestClient[IO],
    attType:     AttachmentType,
    files:       List[DomFile],
    onSuccess:   Attachment.Id => Callback
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
              None,
              dom.readReadableStream(IO(f.stream()))
            )
            .toastErrors
            .flatMap(id =>
              IO.now().flatMap { now =>
                (onSuccess(id) *>
                  attachments
                    .mod(
                      _.updated(
                        id,
                        Attachment(
                          id,
                          attType,
                          name,
                          None,
                          false,
                          f.size.toLong,
                          Timestamp.unsafeFromInstantTruncated(now)
                        )
                      )
                    )).toAsync
              }
            )
        }
      )
      .orEmpty

  def onInsertFileSelected(
    programId:      Program.Id,
    obsAttachments: View[AttachmentList],
    newAttType:     AttachmentType,
    client:         OdbRestClient[IO],
    action:         View[Action],
    onSuccess:      Attachment.Id => Callback = _ => Callback.empty
  )(e: ReactEventFromInput)(using
    ToastCtx[IO],
    Logger[IO]
  ): Callback =
    val files = e.target.files.toList
    (Callback(e.target.value = null) *>
      insertAttachment(programId, obsAttachments, client, newAttType, files, onSuccess)
        .switching(action.async, Action.Insert, Action.None)
        .runAsync)
      .when_(files.nonEmpty)

  def getAttachmentUrl(
    client: OdbRestClient[IO],
    mapKey: UrlMapKey,
    urlMap: View[UrlMap]
  ): IO[Unit] =
    client
      .getAttachmentUrl(mapKey._1)
      .attempt
      .map {
        case Right(url) => Pot(url)
        case Left(t)    => Pot.error(t)
      }
      .flatMap(p => urlMap.mod(_.updated(mapKey, p)).toAsync)

object ObsAttachmentUtils extends ObsAttachmentUtils
