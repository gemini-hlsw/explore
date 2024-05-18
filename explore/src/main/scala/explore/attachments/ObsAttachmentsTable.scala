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
import explore.model.syntax.all.*
import explore.syntax.ui.*
import explore.utils.*
import explore.utils.OdbRestClient
import fs2.dom
import japgolly.scalajs.react.*
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.ObsAttachment as ObsAtt
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.util.Enumerated
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
import lucuma.schemas.enums.ObsAttachmentType
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

case class ObsAttachmentsTable(
  pid:                      Program.Id,
  authToken:                NonEmptyString,
  obsAttachments:           View[ObsAttachmentList],
  obsAttachmentAssignments: ObsAttachmentAssignmentMap,
  readonly:                 Boolean,
  renderInTitle:            Tile.RenderInTitle
) extends ReactFnProps(ObsAttachmentsTable.component)

object ObsAttachmentsTable extends ObsAttachmentUtils:
  private type Props = ObsAttachmentsTable

  private val ColDef = ColumnDef[View[ObsAttachment]]

  given Reusability[UrlMap]                     = Reusability.map
  given Reusability[ObsAttachmentAssignmentMap] = Reusability.map

  def updateAttachment(
    props:  Props,
    client: OdbRestClient[IO],
    oa:     ObsAttachment,
    files:  List[DomFile]
  )(using
    ToastCtx[IO]
  ): IO[Unit] =
    files.headOption
      .map(f =>
        checkFileSize(f) {
          val name = NonEmptyString.unsafeFrom(f.name)
          client
            .updateObsAttachment(props.pid,
                                 oa.id,
                                 name,
                                 oa.description,
                                 dom.readReadableStream(IO(f.stream()))
            ) *>
            IO.now()
              .flatMap { now =>
                props.obsAttachments
                  .mod(
                    _.updatedWith(oa.id)(
                      _.map(
                        _.copy(fileName = name,
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
    aid:    ObsAtt.Id
  )(using ToastCtx[IO]): IO[Unit] =
    props.obsAttachments.mod(_.removed(aid)).toAsync *>
      client.deleteObsAttachment(props.pid, aid).toastErrors

  def deletePrompt(
    props:  Props,
    client: OdbRestClient[IO],
    oa:     ObsAttachment,
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
      .useMemoBy((p, _) => p.authToken)((_, ctx) =>
        token => OdbRestClient[IO](ctx.environment, token)
      )
      .useStateView(Action.None)
      .useStateView[UrlMap](Map.empty)
      .useEffectWithDepsBy((props, _, _, _, _) => props.obsAttachments.get)(
        (props, _, client, _, urlMap) =>
          obsAttachments =>
            val allCurrentKeys = obsAttachments.values.map(_.toMapKey).toSet
            val newOas         = allCurrentKeys.filter(key => !urlMap.get.contains(key)).toList

            val updateUrlMap =
              urlMap.mod { umap =>
                val filteredMap = umap.filter((k, _) => allCurrentKeys.contains(k))
                newOas.foldRight(filteredMap)((key, m) => m.updated(key, Pot.pending))
              }.toAsync
            val getUrls      =
              newOas.traverse_(key => getAttachmentUrl(props.pid, client, key, urlMap))

            updateUrlMap *> getUrls
      )
      // Columns
      .useMemoBy((props, _, client, _, urlMap) =>
        (client, props.obsAttachmentAssignments, urlMap.get, props.readonly)
      )((props, ctx, _, action, _) =>
        (client, assignments, urlMap, readonly) =>
          import ctx.given

          def column[V](id: ColumnId, accessor: ObsAttachment => V)
            : ColumnDef.Single.NoMeta[View[ObsAttachment], V] =
            ColDef(id, v => accessor(v.get), columnNames(id))

          def goToObs(obsId: Observation.Id): Callback =
            ctx.pushPage(AppTab.Observations, props.pid, Focused.singleObs(obsId))

          def obsUrl(obsId: Observation.Id): String =
            ctx.pageUrl(AppTab.Observations, props.pid, Focused.singleObs(obsId))

          List(
            column(ActionsColumnId, identity)
              .setCell(cell =>
                val thisOa = cell.value
                val id     = thisOa.id

                def onUpdateFileSelected(e: ReactEventFromInput): Callback =
                  val files = e.target.files.toList
                  (Callback(e.target.value = null) *>
                    updateAttachment(props, client, thisOa, files)
                      .switching(action.async, Action.Replace, Action.None)
                      .runAsync)
                    .when_(files.nonEmpty)

                <.div(
                  // The upload "button" needs to be a label. In order to make
                  // the styling consistent they're all labels.
                  <.label(
                    tableLabelButtonClasses,
                    Icons.Trash,
                    ^.onClick ==> deletePrompt(props, client, thisOa, assignments.get(id).orEmpty)
                  ).withTooltip("Delete attachment").unless(readonly),
                  <.label(
                    tableLabelButtonClasses,
                    ^.htmlFor := s"attachment-replace-$id",
                    Icons.FileArrowUp
                  ).withTooltip(
                    tooltip = s"Upload replacement file",
                    placement = Placement.Right
                  ).unless(readonly),
                  <.input(
                    ExploreStyles.FileUpload,
                    ^.tpe    := "file",
                    ^.onChange ==> onUpdateFileSelected,
                    ^.id     := s"attachment-replace-$id",
                    ^.name   := "file",
                    ^.accept := thisOa.attachmentType.accept
                  ).unless(readonly),
                  urlMap.get(thisOa.toMapKey).foldMap {
                    case Pot.Ready(url) =>
                      <.a(Icons.FileArrowDown, ^.href := url, tableLabelButtonClasses)
                        .withTooltip("Download File")
                    case Pot.Pending    => <.span(Icons.Spinner.withSpin(true))
                    case Pot.Error(t)   =>
                      <.span(Icons.ExclamationTriangle).withTooltip(t.getMessage)
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
            column(ObservationsColumnId, ObsAttachment.id.get)
              .setCell(cell =>
                <.span(
                  assignments
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
              )
              .setEnableSorting(false),
            ColDef(
              DescriptionColumnId,
              _.withOnMod(oa =>
                ProgramQueries
                  .updateObsAttachmentDescription[IO](oa.id, oa.description)
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
                  discardButtonTooltip = "Discard".some,
                  readonly = readonly
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
                        .updateObsAttachmentChecked[IO](oa.id, oa.checked)
                        .runAsync
                    )
                    .zoom(ObsAttachment.checked),
                  label = "",
                  disabled = readonly
                )
            ).sortableBy(_.get.checked)
          )
      )
      // Rows
      .useMemoBy((props, _, _, _, _, _) => props.obsAttachments.reuseByValue)((_, _, _, _, _, _) =>
        _.value.toListOfViews.map(_._2)
      )
      .useReactTableBy((prop, _, _, _, _, cols, rows) =>
        TableOptions(
          cols,
          rows,
          getRowId = (row, _, _) => RowId(row.get.id.toString)
        )
      )
      .useStateView(Enumerated[ObsAttachmentType].all.head)
      .render { (props, ctx, client, action, _, _, _, table, newAttType) =>
        import ctx.given

        React.Fragment(
          props.renderInTitle(
            if (props.readonly) EmptyVdom
            else
              <.div(
                ExploreStyles.TableSelectionToolbar,
                EnumDropdownView(
                  id = "attachment-type".refined,
                  value = newAttType,
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
                  ^.onChange ==> onInsertFileSelected(props.pid,
                                                      props.obsAttachments,
                                                      newAttType.get,
                                                      client,
                                                      action
                  ),
                  ^.id     := "attachment-upload",
                  ^.name   := "file",
                  ^.accept := newAttType.get.accept
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
            header = action.get.msg,
            blockScroll = true,
            modal = true,
            dismissableMask = false,
            closable = false,
            closeOnEscape = false,
            showHeader = true
          )("Please wait...")
        )
      }
