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
import explore.components.ui.ExploreStyles
import explore.components.Tile
import explore.model.AppContext
import explore.model.Constants
import explore.model.Focused
import explore.model.ObsAttachment
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
import java.time.ZoneOffset
import scala.collection.immutable.SortedSet

case class ObsAttachmentsTable(
  pid:            Program.Id,
  client:         OdbRestClient[IO],
  obsAttachments: View[List[ObsAttachment]],
  renderInTitle:  Tile.RenderInTitle
) extends ReactFnProps(ObsAttachmentsTable.component)

object ObsAttachmentsTable extends TableHooks:
  private type Props = ObsAttachmentsTable

  private type UrlMapKey = (ObsAtt.Id, Timestamp)
  private type UrlMap    = Map[UrlMapKey, Pot[String]]

  private enum Action:
    case None, Insert, Replace, Download

  private val ColDef = ColumnDef[View[ObsAttachment]]

  private val ActionsColumnId: ColumnId        = ColumnId("actions")
  private val FileNameColumnId: ColumnId       = ColumnId("filename")
  private val AttachmentTypeColumnId: ColumnId = ColumnId("attachment-type")
  private val SizeColumnId                     = ColumnId("filesize")
  private val LastUpdateColumnId               = ColumnId("last-update")
  private val ObservationsColumnId: ColumnId   = ColumnId("observations")
  private val DescriptionColumnId: ColumnId    = ColumnId("description")
  private val CheckedColumnId: ColumnId        = ColumnId("checked")

  private val columnNames: Map[ColumnId, String] = Map(
    ActionsColumnId        -> "Actions",
    FileNameColumnId       -> "File",
    AttachmentTypeColumnId -> "Type",
    SizeColumnId           -> "Size",
    LastUpdateColumnId     -> "LastUpdate",
    ObservationsColumnId   -> "Observations",
    DescriptionColumnId    -> "Description",
    CheckedColumnId        -> "Checked"
  )

  extension (oa: ObsAttachment) def toMapKey: UrlMapKey = (oa.id, oa.updatedAt)

  private val labelButtonClasses =
    PrimeStyles.Component |+| PrimeStyles.Button |+| PrimeStyles.ButtonIconOnly
      |+| LucumaStyles.Tiny |+| LucumaStyles.Compact

  private val tableLabelButtonClasses = labelButtonClasses |+| PrimeStyles.ButtonSecondary

  // TEMPORARY until we get the graphql enums worked out
  given Enumerated[ObsAttachmentType] =
    Enumerated
      .from(ObsAttachmentType.Finder, ObsAttachmentType.MosMask, ObsAttachmentType.PreImaging)
      .withTag(_.toString)
  given Display[ObsAttachmentType]    = Display.byShortName(_.toString)

  given Reusability[UrlMap] = Reusability.map

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

  def insertAttachment(props: Props, attType: ObsAttachmentType, files: List[DomFile])(using
    ToastCtx[IO]
  ): IO[Unit] =
    files.headOption
      .map(f =>
        checkFileSize(f) {
          val name = NonEmptyString.unsafeFrom(f.name)
          props.client
            .insertObsAttachment(props.pid,
                                 attType,
                                 name,
                                 None,
                                 dom.readReadableStream(IO(f.stream()))
            )
            .flatMap(id =>
              props.obsAttachments
                .mod(
                  _ :+ ObsAttachment(
                    id,
                    attType,
                    name,
                    None,
                    false,
                    f.size.toLong,
                    Timestamp.unsafeFromInstantTruncated(Instant.now()),
                    SortedSet.empty
                  )
                )
                .to[IO]
            )
            .toastErrors
        }
      )
      .orEmpty

  def updateAttachment(props: Props, oa: ObsAttachment, files: List[DomFile])(using
    ToastCtx[IO]
  ): IO[Unit] =
    files.headOption
      .map(f =>
        checkFileSize(f) {
          val name = NonEmptyString.unsafeFrom(f.name)
          props.client
            .updateObsAttachment(props.pid,
                                 oa.id,
                                 name,
                                 oa.description,
                                 dom.readReadableStream(IO(f.stream()))
            ) *>
            props.obsAttachments
              .mod(
                _.map(o =>
                  if (o.id === oa.id)
                    o.copy(fileName = name,
                           updatedAt = Timestamp.unsafeFromInstantTruncated(Instant.now()),
                           checked = false
                    )
                  else o
                )
              )
              .to[IO]
              .toastErrors
        }
      )
      .orEmpty

  def deleteAttachment(
    props: Props,
    aid:   ObsAtt.Id
  )(using ToastCtx[IO]): IO[Unit] =
    props.obsAttachments.mod(_.filter(_.id =!= aid)).to[IO] *>
      props.client.deleteAttachment(props.pid, aid).toastErrors

  def getAttachmentUrl(
    props:  Props,
    oa:     ObsAttachment,
    urlMap: View[UrlMap]
  ): IO[Unit] =
    props.client
      .getPresignedUrl(props.pid, oa.id)
      .attempt
      .map {
        case Right(url) => Pot(url)
        case Left(t)    => Pot.error(t)
      }
      .flatMap(p => urlMap.mod(_.updated(oa.toMapKey, p)).to[IO])

  def deletePrompt(props: Props, oa: ObsAttachment)(
    e: ReactMouseEvent
  )(using Logger[IO], ToastCtx[IO]): Callback =
    val msg =
      if (oa.observations.isEmpty) ""
      else s"It is assigned to ${oa.observations.size} observations. "
    ConfirmPopup
      .confirmPopup(
        e.currentTarget.domAsHtml,
        s"Delete attachment? ${msg}This action is not undoable.",
        acceptLabel = "Delete",
        rejectLabel = "Cancel",
        accept = deleteAttachment(props, oa.id).runAsync
      )
      .show

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateView(Action.None)
      .useStateView[UrlMap](Map.empty)
      .useEffectWithDepsBy((props, _, _, _) => props.obsAttachments.get)((props, _, _, urlMap) =>
        obsAttachments =>
          val newOas         = obsAttachments.filter(oa => !urlMap.get.contains(oa.toMapKey))
          val allCurrentKeys = obsAttachments.map(_.toMapKey).toSet

          val updateUrlMap =
            urlMap
              .mod { umap =>
                val filteredMap = umap.filter((k, v) => allCurrentKeys.contains(k))
                newOas.foldRight(filteredMap)((oa, m) => m.updated(oa.toMapKey, Pot.pending))
              }
              .to[IO]
          val getUrls      =
            newOas.traverse_(oa => getAttachmentUrl(props, oa, urlMap))

          updateUrlMap *> getUrls
      )
      // Columns
      .useMemoBy((_, _, _, urlMap) => urlMap.get)((props, ctx, action, _) =>
        urlMap =>
          import ctx.given

          def column[V](id: ColumnId, accessor: ObsAttachment => V)
            : ColumnDef.Single[View[ObsAttachment], V] =
            ColDef(id, v => accessor(v.get), columnNames(id))

          def goToObs(obsId: Observation.Id): Callback =
            ctx.pushPage(AppTab.Constraints, props.pid, Focused.singleObs(obsId))

          def obsUrl(obsId: Observation.Id): String =
            ctx.pageUrl(AppTab.Constraints, props.pid, Focused.singleObs(obsId))

          List(
            column(ActionsColumnId, identity)
              .setCell(cell =>
                val thisOa = cell.value
                val id     = thisOa.id

                def onUpdateFileSelected(e: ReactEventFromInput): Callback =
                  val files = e.target.files.toList
                  (Callback(e.target.value = null) *>
                    action.set(Action.Replace) *>
                    updateAttachment(props, thisOa, files)
                      .guarantee(action.async.set(Action.None))
                      .runAsync())
                    .when_(files.nonEmpty)

                <.div(
                  // The upload "button" needs to be a label. In order to make
                  // the styling consistent they're all labels.
                  <.label(
                    tableLabelButtonClasses,
                    Icons.Trash,
                    ^.onClick ==> deletePrompt(props, thisOa)
                  ).withTooltip("Delete attachment"),
                  <.label(
                    tableLabelButtonClasses,
                    ^.htmlFor := s"attachment-replace-$id",
                    Icons.FileArrowUp
                  ).withTooltip(
                    tooltip = s"Upload replacement file",
                    placement = Placement.Right
                  ),
                  <.input(
                    ExploreStyles.FileUpload,
                    ^.tpe  := "file",
                    ^.onChange ==> onUpdateFileSelected,
                    ^.id   := s"attachment-replace-$id",
                    ^.name := "file"
                  ),
                  urlMap.get(thisOa.toMapKey).foldMap {
                    case Pot.Ready(url) =>
                      <.a(Icons.FileArrowDown, ^.href := url, tableLabelButtonClasses)
                        .withTooltip("Download File")
                    case Pot.Pending    => <.span(Icons.Spinner.withSpin(true))
                    case Pot.Error(t)   => <.span(Icons.ExclamationTriangle).withTooltip(t.getMessage)
                  }
                )
              )
              .setEnableSorting(false),
            column(FileNameColumnId, ObsAttachment.fileName.get)
              .setCell(_.value.value)
              .sortableBy(_.value.toUpperCase),
            column(AttachmentTypeColumnId, ObsAttachment.attachmentType.get)
              .setCell(_.value.shortName),
            column(SizeColumnId, ObsAttachment.fileSize.get)
              .setCell(cell =>
                // The fileSize will always be > 0, the api should be changed to reflect this
                NonNegLong.from(cell.value).toOption.map(_.toHumanReadableByteCount).orEmpty
              ),
            column(LastUpdateColumnId, ObsAttachment.updatedAt.get)
              .setCell(cell =>
                Constants.GppDateFormatter
                  .format(cell.value.toLocalDateTime)
              ),
            column(ObservationsColumnId, ObsAttachment.observations.get)
              .setCell(cell =>
                <.span(
                  cell.value.toList
                    .map(obsId =>
                      <.a(
                        ^.href := obsUrl(obsId),
                        ^.onClick ==> (_.preventDefaultCB >> goToObs(obsId)),
                        obsId.toString
                      )
                    )
                    .mkReactFragment(", ")
                )
              )
              .setEnableSorting(false),
            ColDef(
              DescriptionColumnId,
              _.withOnMod(oa =>
                ProgramQueries
                  .updateObsAttachmentDescription[IO](props.pid, oa.id, oa.description)
                  .runAsync
              )
                .zoom(ObsAttachment.description),
              columnNames(DescriptionColumnId),
              cell =>
                EditableLabel.fromView(
                  value = cell.value,
                  addButtonLabel = ("Add description": VdomNode).reuseAlways,
                  textClass = ExploreStyles.AttachmentName,
                  inputClass = ExploreStyles.AttachmentNameInput,
                  editButtonTooltip = "Edit description".some,
                  deleteButtonTooltip = "Delete description".some,
                  okButtonTooltip = "Accept".some,
                  discardButtonTooltip = "Discard".some
                )
            )
              .sortableBy(_.get.map(_.value.toUpperCase).orEmpty),
            ColDef(
              CheckedColumnId,
              identity,
              columnNames(CheckedColumnId),
              cell =>
                CheckboxView(
                  id = NonEmptyString.unsafeFrom(s"checked-${cell.value.get.id}"),
                  value = cell.value
                    .withOnMod(oa =>
                      ProgramQueries
                        .updateObsAttachmentChecked[IO](props.pid, oa.id, oa.checked)
                        .runAsync
                    )
                    .zoom(ObsAttachment.checked),
                  label = ""
                )
            ).sortableBy(_.get.checked)
          )
      )
      // Rows
      .useMemoBy((props, _, _, _, _) => props.obsAttachments.reuseByValue)((_, _, _, _, _) =>
        _.value.toListOfViews.sortBy(_.get.id)
      )
      .useReactTableBy((prop, ctx, _, _, cols, rows) =>
        TableOptions(
          cols,
          rows,
          getRowId = (row, _, _) => RowId(row.get.id.toString)
        )
      )
      .useStateView(Enumerated[ObsAttachmentType].all.head)
      .render { (props, ctx, action, _, _, _, table, newAttType) =>
        import ctx.given

        val dialogHeader = action.get match
          case Action.None     => ""
          case Action.Insert   => "Uploading Attachment"
          case Action.Replace  => "Uploading Replacement"
          case Action.Download => "Downloading Attachment"

        def onInsertFileSelected(e: ReactEventFromInput): Callback =
          val files = e.target.files.toList
          (Callback(e.target.value = null) *>
            action.set(Action.Insert) *>
            insertAttachment(props, newAttType.get, files)
              .guarantee(action.async.set(Action.None))
              .runAsync)
            .when_(files.nonEmpty)

        React.Fragment(
          props.renderInTitle(
            <.div(
              ExploreStyles.TableSelectionToolbar,
              EnumDropdownView(
                id = "attachment-type".refined,
                value = newAttType,
                clazz = ExploreStyles.FlatFormField |+| ExploreStyles.AttachmentsTableTypeSelect
              ),
              <.label(
                labelButtonClasses,
                ^.htmlFor := "attachment-upload",
                Icons.FileArrowUp
              ).withTooltip(
                tooltip = s"Upload new ${newAttType.get.shortName} attachment",
                placement = Placement.Right
              ),
              <.input(
                ExploreStyles.FileUpload,
                ^.tpe  := "file",
                ^.onChange ==> onInsertFileSelected,
                ^.id   := "attachment-upload",
                ^.name := "file"
              )
            )
          ),
          PrimeTable(
            table,
            striped = true,
            compact = Compact.Very,
            emptyMessage = <.div("No observation attachments uploaded"),
            tableMod = ExploreStyles.AttachmentsTable
          ),
          ConfirmPopup(),
          Dialog(
            onHide = Callback.empty,
            visible = action.get != Action.None,
            header = dialogHeader,
            blockScroll = true,
            modal = true,
            dismissableMask = false,
            closable = false,
            closeOnEscape = false,
            showHeader = true
          )("Please wait...")
        )
      }
