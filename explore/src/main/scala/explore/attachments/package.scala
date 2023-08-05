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
import explore.model.ObsAttachment
import explore.model.ObsAttachmentList
import explore.syntax.ui.*
import explore.utils.OdbRestClient
import explore.utils.*
import fs2.dom
import japgolly.scalajs.react.*
import lucuma.core.model.Program
import lucuma.core.model.{ObsAttachment => ObsAtt}
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.util.Timestamp
import lucuma.react.primereact.Message
import lucuma.react.primereact.PrimeStyles
import lucuma.react.table.ColumnId
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Enums.ObsAttachmentType
import lucuma.ui.primereact.LucumaPrimeStyles
import org.scalajs.dom.{File => DomFile}
import org.typelevel.log4cats.Logger

import java.time.Instant

// TEMPORARY until we get the graphql enums worked out
enum AttachmentType(
  val tag:        String,
  val name:       String,
  val gql:        ObsAttachmentType,
  val extensions: List[String]
) derives Enumerated {
  case Finder
      extends AttachmentType("FINDER",
                             "Finder Chart",
                             ObsAttachmentType.Finder,
                             List("jpeg", "jpg", "png")
      )
  case MosMask
      extends AttachmentType("MOS_MASK", "MOS Mask", ObsAttachmentType.MosMask, List("fits"))
  case PreImaging
      extends AttachmentType("PRE_IMAGING",
                             "Pre-Imaging",
                             ObsAttachmentType.PreImaging,
                             List("fits")
      )

  def accept: String = extensions.map("." + _).mkString(",")
}

trait ObsAttachmentUtils:
  type UrlMapKey = (ObsAtt.Id, Timestamp)
  type UrlMap    = Map[UrlMapKey, Pot[String]]

  val AttIdColumnId: ColumnId          = ColumnId("id")
  val ActionsColumnId: ColumnId        = ColumnId("actions")
  val FileNameColumnId: ColumnId       = ColumnId("filename")
  val AttachmentTypeColumnId: ColumnId = ColumnId("attachment-type")
  val SizeColumnId                     = ColumnId("filesize")
  val LastUpdateColumnId               = ColumnId("last-update")
  val ObservationsColumnId: ColumnId   = ColumnId("observations")
  val DescriptionColumnId: ColumnId    = ColumnId("description")
  val CheckedColumnId: ColumnId        = ColumnId("checked")

  given Display[AttachmentType] = Display.byShortName(_.name)

  val LabelButtonClasses =
    PrimeStyles.Component |+| PrimeStyles.Button |+| PrimeStyles.ButtonIconOnly
      |+| LucumaPrimeStyles.Tiny |+| LucumaPrimeStyles.Compact

  enum Action derives Eq:
    case None, Insert, Replace, Download, Unlink

  // TODO: Maybe we can have a graphql query for getting information such as this? This is a config var in ODB.
  private val maxFileSize: NonNegLong = 10000000.refined

  extension (oa: ObsAttachment) def toMapKey: UrlMapKey = (oa.id, oa.updatedAt)

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

  def insertAttachment(
    programId:      Program.Id,
    obsAttachments: View[ObsAttachmentList],
    client:         OdbRestClient[IO],
    attType:        ObsAttachmentType,
    files:          List[DomFile],
    onSuccess:      ObsAtt.Id => Callback
  )(using
    ToastCtx[IO]
  ): IO[Unit] =
    files.headOption
      .map(f =>
        checkFileSize(f) {
          val name = NonEmptyString.unsafeFrom(f.name)
          client
            .insertObsAttachment(programId,
                                 attType,
                                 name,
                                 None,
                                 dom.readReadableStream(IO(f.stream()))
            )
            .toastErrors
            .flatMap(id =>
              (onSuccess(id) *>
                obsAttachments
                  .mod(
                    _.updated(id,
                              ObsAttachment(
                                id,
                                attType,
                                name,
                                None,
                                false,
                                f.size.toLong,
                                Timestamp.unsafeFromInstantTruncated(Instant.now())
                              )
                    )
                  )).toAsync
            )
        }
      )
      .orEmpty

  def onInsertFileSelected(
    programId:      Program.Id,
    obsAttachments: View[ObsAttachmentList],
    newAttType:     AttachmentType,
    client:         OdbRestClient[IO],
    action:         View[Action],
    onSuccess:      ObsAtt.Id => Callback = _ => Callback.empty
  )(e: ReactEventFromInput)(using
    ToastCtx[IO],
    Logger[IO]
  ): Callback =
    val files = e.target.files.toList
    (Callback(e.target.value = null) *>
      insertAttachment(programId, obsAttachments, client, newAttType.gql, files, onSuccess)
        .switching(action.async, Action.Insert, Action.None)
        .runAsync)
      .when_(files.nonEmpty)

  def getAttachmentUrl(
    pid:    Program.Id,
    client: OdbRestClient[IO],
    mapKey: UrlMapKey,
    urlMap: View[UrlMap]
  ): IO[Unit] =
    client
      .getPresignedUrl(pid, mapKey._1)
      .attempt
      .map {
        case Right(url) => Pot(url)
        case Left(t)    => Pot.error(t)
      }
      .flatMap(p => urlMap.mod(_.updated(mapKey, p)).toAsync)

object ObsAttachmentUtils extends ObsAttachmentUtils
