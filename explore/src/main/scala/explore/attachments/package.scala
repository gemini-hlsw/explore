// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.attachments

import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.react.*
import crystal.react.hooks.*
import crystal.react.implicits.*
import crystal.react.reuse.*
import eu.timepit.refined.types.numeric.NonNegLong
import eu.timepit.refined.types.string.NonEmptyString
import explore.EditableLabel
import explore.Icons
import explore.common.ProgramQueries
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Constants
import explore.model.Focused
import explore.model.ObsAttachment
import explore.model.ObsAttachmentAssignmentMap
import explore.model.ObsAttachmentList
import explore.model.enums.AppTab
import explore.model.reusability.given
import explore.syntax.ui.*
import explore.utils.OdbRestClient
import explore.utils.*
import fs2.dom
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.{ObsAttachment => ObsAtt}
import lucuma.core.syntax.all.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.util.NewType
import lucuma.core.util.Timestamp
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Enums.ObsAttachmentType
import lucuma.ui.primereact.CheckboxView
import lucuma.ui.primereact.EnumDropdownView
import lucuma.ui.primereact.LucumaStyles
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import lucuma.ui.utils.*
import monocle.Iso
import monocle.Lens
import org.scalajs.dom.{File => DomFile}
import org.typelevel.log4cats.Logger
import react.common.ReactFnProps
import react.common.style.Css
import react.floatingui.Placement
import react.floatingui.syntax.*
import react.primereact.Button
import react.primereact.ConfirmPopup
import react.primereact.Dialog
import react.primereact.Message
import react.primereact.PrimeStyles

import java.time.Instant
import scala.collection.immutable.SortedSet

// TEMPORARY until we get the graphql enums worked out
enum AttachmentType(
  val tag:        String,
  val name:       String,
  val gql:        ObsAttachmentType,
  val extensions: List[String]
) derives Enumerated {
  case Finder
      extends AttachmentType("FINDER", "Finder Chart", ObsAttachmentType.Finder, List("jpg", "png"))
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

  given Display[AttachmentType] = Display.byShortName(_.name)

  enum Action:
    case None, Insert, Replace, Download

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
                  ))
                .to[IO]
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
      action.set(Action.Insert) *>
      insertAttachment(programId, obsAttachments, client, newAttType.gql, files, onSuccess)
        .guarantee(action.async.set(Action.None))
        .runAsync)
      .when_(files.nonEmpty)
