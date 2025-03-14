// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.attachments

import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.react.*
import crystal.react.given
import crystal.react.hooks.*
import crystal.react.reuse.*
import crystal.syntax.*
import eu.timepit.refined.types.numeric.NonNegLong
import eu.timepit.refined.types.string.NonEmptyString
import explore.EditableLabel
import explore.Icons
import explore.common.ProgramQueries
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Attachment
import explore.model.AttachmentList
import explore.model.Constants
import explore.model.Focused
import explore.model.ObsAttachmentAssignmentMap
import explore.model.Observation
import explore.model.display.given
import explore.model.enums.AppTab
import explore.model.syntax.all.*
import explore.syntax.ui.*
import explore.utils.*
import fs2.dom
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.AttachmentPurpose
import lucuma.core.enums.AttachmentType
import lucuma.core.model.Program
import lucuma.core.util.Enumerated
import lucuma.core.util.NewType
import lucuma.core.util.Timestamp
import lucuma.react.common.ReactFnProps
import lucuma.react.common.style.Css
import lucuma.react.floatingui.Placement
import lucuma.react.floatingui.syntax.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.ConfirmPopup
import lucuma.react.primereact.Dialog
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.ui.primereact.CheckboxView
import lucuma.ui.primereact.EnumDropdownView
import lucuma.ui.primereact.given
import lucuma.ui.react.given
import lucuma.ui.reusability.given
import lucuma.ui.table.*
import lucuma.ui.utils.*
import org.scalajs.dom.File as DomFile
import org.typelevel.log4cats.Logger

import scala.collection.immutable.SortedSet

object ObsAttachmentsTableTileState extends NewType[Action]:
  def apply(): ObsAttachmentsTableTileState = ObsAttachmentsTableTileState(Action.None)
type ObsAttachmentsTableTileState = ObsAttachmentsTableTileState.Type

case class ObsAttachmentsTableBody(
  pid:                      Program.Id,
  authToken:                NonEmptyString,
  obsAttachmentAssignments: ObsAttachmentAssignmentMap,
  attachments:              View[AttachmentList],
  readOnly:                 Boolean,
  tileState:                View[ObsAttachmentsTableTileState]
) extends ReactFnProps(ObsAttachmentsTableBody.component):
  val action = tileState.zoom(ObsAttachmentsTableTileState.value.asLens)

object ObsAttachmentsTableBody extends ObsAttachmentUtils:
  private type Props = ObsAttachmentsTableBody

  private case class TableMeta(
    client:      OdbRestClient[IO],
    assignments: ObsAttachmentAssignmentMap,
    urlMap:      UrlMap,
    readOnly:    Boolean
  )

  private val ColDef = ColumnDef.WithTableMeta[View[Attachment], TableMeta]

  given Reusability[UrlMap]                     = Reusability.map
  given Reusability[ObsAttachmentAssignmentMap] = Reusability.map

  def updateAttachment(
    props:  Props,
    client: OdbRestClient[IO],
    oa:     Attachment,
    files:  List[DomFile]
  )(using
    ToastCtx[IO]
  ): IO[Unit] =
    files.headOption
      .map(f =>
        checkFileSize(f) {
          val name = NonEmptyString.unsafeFrom(f.name)
          client
            .updateAttachment(
              oa.id,
              name,
              oa.description,
              dom.readReadableStream(IO(f.stream()))
            ) *>
            IO.now()
              .flatMap { now =>
                props.attachments
                  .mod(
                    _.updatedWith(oa.id)(
                      _.map(
                        _.copy(
                          fileName = name,
                          updatedAt = Timestamp.unsafeFromInstantTruncated(now),
                          checked = false
                        )
                      )
                    )
                  )
                  .toAsync
              }
              .toastErrors
        }
      )
      .orEmpty

  def deleteAttachment(
    props:  Props,
    client: OdbRestClient[IO],
    aid:    Attachment.Id
  )(using ToastCtx[IO]): IO[Unit] =
    props.attachments.mod(_.removed(aid)).toAsync *>
      client.deleteAttachment(aid).toastErrors

  def deletePrompt(
    props:  Props,
    client: OdbRestClient[IO],
    oa:     Attachment,
    obsIds: SortedSet[Observation.Id]
  )(
    e:      ReactMouseEvent
  )(using Logger[IO], ToastCtx[IO]): Callback =
    val msg =
      if (obsIds.isEmpty) ""
      else s"It is assigned to ${obsIds.size} observations. "
    ConfirmPopup
      .confirmPopup(
        e.currentTarget.domAsHtml,
        s"Delete attachment? ${msg}This action is not undoable.",
        acceptLabel = "Delete",
        rejectLabel = "Cancel",
        accept = deleteAttachment(props, client, oa.id).runAsync
      )
      .show

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useMemoBy((p, _) => p.authToken): (_, ctx) => // client
        token => OdbRestClient[IO](ctx.environment, token) // This could be in the shared state
      .useStateView[UrlMap](Map.empty) // urlMap
      .useEffectWithDepsBy((props, _, _, _) => props.attachments.get): (_, _, client, urlMap) =>
        attachments =>
          val allCurrentKeys = attachments.observationList.map(_.toMapKey).toSet
          val newOas         = allCurrentKeys.filter(key => !urlMap.get.contains(key)).toList

          val updateUrlMap =
            urlMap.mod { umap =>
              val filteredMap = umap.filter((k, _) => allCurrentKeys.contains(k))
              newOas.foldRight(filteredMap)((key, m) => m.updated(key, pending))
            }.toAsync
          val getUrls      =
            newOas.traverse_(key => getAttachmentUrl(client, key, urlMap))

          updateUrlMap *> getUrls
      // Columns
      .useMemoBy((_, _, _, _) => ()): (props, ctx, _, _) =>
        _ =>
          import ctx.given

          def column[V](id: ColumnId, accessor: Attachment => V)
            : ColumnDef.Single.WithTableMeta[View[Attachment], V, TableMeta] =
            ColDef(id, v => accessor(v.get), columnNames(id))

          def goToObs(obsId: Observation.Id): Callback =
            ctx.pushPage((AppTab.Observations, props.pid, Focused.singleObs(obsId)).some)

          def obsUrl(obsId: Observation.Id): String =
            ctx.pageUrl((AppTab.Observations, props.pid, Focused.singleObs(obsId)).some)

          List(
            column(ActionsColumnId, identity)
              .setCell: cell =>
                cell.table.options.meta.map: meta =>
                  val thisOa: Attachment = cell.value
                  val id: Attachment.Id  = thisOa.id

                  def onUpdateFileSelected(e: ReactEventFromInput): Callback =
                    val files = e.target.files.toList
                    (Callback(e.target.value = null) *>
                      updateAttachment(props, meta.client, thisOa, files)
                        .switching(props.action.async, Action.Replace, Action.None)
                        .runAsync)
                      .when_(files.nonEmpty)

                  <.div(
                    // The upload "button" needs to be a label. In order to make
                    // the styling consistent they're all labels.
                    <.label(
                      tableLabelButtonClasses,
                      Icons.Trash,
                      ^.onClick ==> deletePrompt(
                        props,
                        meta.client,
                        thisOa,
                        meta.assignments.get(id).orEmpty
                      )
                    ).withTooltip("Delete attachment").unless(meta.readOnly),
                    <.label(
                      tableLabelButtonClasses,
                      ^.htmlFor := s"attachment-replace-$id",
                      Icons.FileArrowUp
                    ).withTooltip(
                      tooltip = s"Upload replacement file",
                      placement = Placement.Right
                    ).unless(meta.readOnly),
                    <.input(
                      ExploreStyles.FileUpload,
                      ^.tpe    := "file",
                      ^.onChange ==> onUpdateFileSelected,
                      ^.id     := s"attachment-replace-$id",
                      ^.name   := "file",
                      ^.accept := thisOa.attachmentType.accept
                    ).unless(meta.readOnly),
                    meta.urlMap
                      .get(thisOa.toMapKey)
                      .foldMap:
                        case Pot.Ready(url) =>
                          <.a(Icons.FileArrowDown, ^.href := url, tableLabelButtonClasses)
                            .withTooltip("Download File")
                        case Pot.Pending    => <.span(Icons.Spinner.withSpin(true))
                        case Pot.Error(t)   =>
                          <.span(Icons.ExclamationTriangle).withTooltip(t.getMessage)
                  )
              .setEnableSorting(false),
            column(FileNameColumnId, Attachment.fileName.get)
              .setCell(_.value.value)
              .sortableBy(_.value.toUpperCase),
            column(AttachmentTypeColumnId, Attachment.attachmentType.get)
              .setCell(_.value.shortName),
            column(SizeColumnId, Attachment.fileSize.get)
              .setCell(cell =>
                // The fileSize will always be > 0, the api should be changed to reflect this
                NonNegLong.from(cell.value).toOption.map(_.toHumanReadableByteCount).orEmpty
              ),
            column(LastUpdateColumnId, Attachment.updatedAt.get)
              .setCell(cell =>
                Constants.GppDateFormatter
                  .format(cell.value.toLocalDateTime)
              ),
            column(ObservationsColumnId, Attachment.id.get)
              .setCell: cell =>
                cell.table.options.meta.map: meta =>
                  <.span(
                    meta.assignments
                      .get(cell.value)
                      .orEmpty
                      .toList
                      .map(obsId =>
                        <.a(
                          ^.href := obsUrl(obsId),
                          ^.onClick ==> (_.preventDefaultCB >> goToObs(obsId)),
                          obsId.toString
                        )
                      )
                      .mkReactFragment(", ")
                  )
              .setEnableSorting(false),
            ColDef(
              DescriptionColumnId,
              _.withOnMod(oa =>
                ProgramQueries
                  .updateAttachmentDescription[IO](oa.id, oa.description)
                  .runAsync
              )
                .zoom(Attachment.description),
              columnNames(DescriptionColumnId),
              cell =>
                cell.table.options.meta.map: meta =>
                  EditableLabel.fromView(
                    value = cell.value,
                    addButtonLabel = ("Add description": VdomNode).reuseAlways,
                    textClass = ExploreStyles.AttachmentName,
                    inputClass = ExploreStyles.AttachmentNameInput,
                    editButtonTooltip = "Edit description".some,
                    deleteButtonTooltip = "Delete description".some,
                    okButtonTooltip = "Accept".some,
                    discardButtonTooltip = "Discard".some,
                    readonly = meta.readOnly
                  )
            )
              .sortableBy(_.get.map(_.value.toUpperCase).orEmpty),
            ColDef(
              CheckedColumnId,
              identity,
              columnNames(CheckedColumnId),
              cell =>
                cell.table.options.meta.map: meta =>
                  CheckboxView(
                    id = NonEmptyString.unsafeFrom(s"checked-${cell.value.get.id}"),
                    value = cell.value
                      .withOnMod(oa =>
                        ProgramQueries
                          .updateAttachmentChecked[IO](oa.id, oa.checked)
                          .runAsync
                      )
                      .zoom(Attachment.checked),
                    label = "",
                    disabled = meta.readOnly
                  )
            ).sortableBy(_.get.checked)
          )
      // Rows
      .useMemoBy((props, _, _, _, _) => props.attachments.reuseByValue): (_, _, _, _, _) =>
        _.value.toListOfViews
          .map(_._2)
          .filter(_.get.isForPurpose(AttachmentPurpose.Observation))
      .useReactTableBy: (props, _, client, urlMap, cols, rows) =>
        TableOptions(
          cols,
          rows,
          getRowId = (row, _, _) => RowId(row.get.id.toString),
          meta = TableMeta(client, props.obsAttachmentAssignments, urlMap.get, props.readOnly)
        )
      .render: (props, _, _, _, _, _, table) =>
        React.Fragment(
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
            visible = props.action.get != Action.None,
            header = props.action.get.msg,
            blockScroll = true,
            modal = true,
            dismissableMask = false,
            closable = false,
            closeOnEscape = false,
            showHeader = true
          )("Please wait...")
        )

case class ObsAttachmentsTableTitle(
  pid:         Program.Id,
  authToken:   NonEmptyString,
  attachments: View[AttachmentList],
  readOnly:    Boolean,
  tileState:   View[ObsAttachmentsTableTileState]
) extends ReactFnProps(ObsAttachmentsTableTitle.component):
  val action = tileState.zoom(ObsAttachmentsTableTileState.value.asLens)

object ObsAttachmentsTableTitle extends ObsAttachmentUtils:
  private type Props = ObsAttachmentsTableTitle

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useMemoBy((p, _) => p.authToken): (_, ctx) => // client
        token => OdbRestClient[IO](ctx.environment, token)
      .useStateView(
        Enumerated[AttachmentType].forPurpose(AttachmentPurpose.Observation).head
      )
      .useMemoBy((_, _, _, _) => ()): (_, _, _, _) =>
        _ => Enumerated[AttachmentType].notForPurpose(AttachmentPurpose.Observation).toSet
      .render: (props, ctx, client, newAttType, excludedTypes) =>
        import ctx.given

        if (props.readOnly) EmptyVdom
        else
          <.div(
            ExploreStyles.TableSelectionToolbar,
            EnumDropdownView(
              id = "attachment-type".refined,
              value = newAttType,
              exclude = excludedTypes,
              clazz = ExploreStyles.FlatFormField |+| ExploreStyles.AttachmentsTableTypeSelect
            ),
            <.label(
              LabelButtonClasses,
              ^.htmlFor := "attachment-upload",
              Icons.FileArrowUp
            ).withTooltip(
              tooltip = s"Upload new ${newAttType.get.shortName} attachment",
              placement = Placement.Right
            ),
            <.input(
              ExploreStyles.FileUpload,
              ^.tpe    := "file",
              ^.onChange ==> onInsertFileSelected(
                props.pid,
                props.attachments,
                newAttType.get,
                client,
                props.action
              ),
              ^.id     := "attachment-upload",
              ^.name   := "file",
              ^.accept := newAttType.get.accept
            )
          )
