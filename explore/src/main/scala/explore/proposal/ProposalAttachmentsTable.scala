// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.Endo
import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.react.*
import crystal.react.hooks.*
import crystal.react.reuse.*
import crystal.syntax.*
import eu.timepit.refined.types.numeric.NonNegLong
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.attachments.Action
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Attachment
import explore.model.AttachmentList
import explore.model.Constants
import explore.model.ProposalType
import explore.model.reusability.given
import explore.model.syntax.all.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.AttachmentPurpose
import lucuma.core.enums.AttachmentType
import lucuma.core.enums.ScienceSubtype
import lucuma.core.model.Program
import lucuma.core.util.Enumerated
import lucuma.core.util.Timestamp
import lucuma.react.common.ReactFnProps
import lucuma.react.common.style.Css
import lucuma.react.floatingui.Placement
import lucuma.react.floatingui.syntax.*
import lucuma.react.primereact.ConfirmPopup
import lucuma.react.primereact.Dialog
import lucuma.react.table.*
import lucuma.ui.react.given
import lucuma.ui.reusability.given
import lucuma.ui.table.*
import org.typelevel.log4cats.Logger

case class ProposalAttachmentsTable(
  programId:    Program.Id,
  authToken:    NonEmptyString,
  attachments:  View[AttachmentList],
  proposalType: Option[ProposalType],
  readOnly:     Boolean
) extends ReactFnProps(ProposalAttachmentsTable.component)

object ProposalAttachmentsTable extends ProposalAttachmentUtils {
  private type Props = ProposalAttachmentsTable

  private type Row = Either[AttachmentType, Attachment]

  extension (row: Row)
    def attachmentType: AttachmentType = row.fold(identity, _.attachmentType)
    def fileName: String               = row.foldMap(_.fileName.value)
    def fileSize: Option[NonNegLong]   = row.flatMap(pa => NonNegLong.from(pa.fileSize)).toOption
    def updatedAt: Option[Timestamp]   = row.toOption.map(_.updatedAt)

  private type UrlMapKey = (Attachment.Id, Timestamp)
  private type UrlMap    = Map[UrlMapKey, Pot[String]]

  private case class TableMeta(action: View[Action], urlMap: UrlMap)

  private val ColDef = ColumnDef[Row].WithTableMeta[TableMeta]

  extension (a: Attachment) private def toMapKey: UrlMapKey = (a.id, a.updatedAt)

  def deletePrompt(
    modAttachments: Endo[AttachmentList] => Callback,
    client:         OdbRestClient[IO],
    att:            Attachment
  )(
    e:              ReactMouseEvent
  )(using Logger[IO], ToastCtx[IO]): Callback =
    ConfirmPopup
      .confirmPopup(
        e.currentTarget.domAsHtml,
        s"Delete attachment? This action is not undoable.",
        acceptLabel = "Delete",
        rejectLabel = "Cancel",
        accept = deleteAttachment(modAttachments, client, att.id).runAsync
      )
      .show

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useMemoBy((p, _) => p.authToken): (_, ctx) =>
        token => OdbRestClient[IO](ctx.environment, token)
      .useStateView(Action.None)
      .useStateView[UrlMap](Map.empty)
      .useEffectWithDepsBy((props, _, _, _, _) => props.attachments.get.proposalList):
        (_, _, client, _, urlMap) =>
          attachments =>
            val allCurrentKeys = attachments.map(_.toMapKey).toSet
            val newPas         = allCurrentKeys.filter(key => !urlMap.get.contains(key)).toList

            val updateUrlMap =
              urlMap.mod { umap =>
                val filteredMap = umap.filter((k, _) => allCurrentKeys.contains(k))
                newPas.foldRight(filteredMap)((key, m) => m.updated(key, pending))
              }.toAsync
            val getUrls      =
              newPas.traverse_(key =>
                getAttachmentUrl(key._1, client).flatMap(p => urlMap.mod(_.updated(key, p)).toAsync)
              )

            updateUrlMap *> getUrls
      .useMemoBy((props, _, client, _, _) => (props.readOnly, client)): // cols
        (props, ctx, _, _, _) =>
          (readOnly, client) =>
            import ctx.given

            def column[V](id: ColumnId, accessor: Row => V)
              : ColumnDef.Single.WithTableMeta[Row, V, TableMeta] =
              ColDef(id, v => accessor(v), columnNames(id))

            List(
              column(ActionsColumnId, identity)
                .withCell(cell =>
                  cell.table.options.meta.map: meta =>
                    cell.value.fold(
                      attType =>
                        <.div(
                          <.label(
                            tableLabelButtonClasses,
                            ExploreStyles.WarningLabel,
                            ^.htmlFor := s"attachment-upload-$attType",
                            Icons.FileArrowUp
                          ).withTooltip(
                            tooltip = s"Upload new ${attType.shortName} attachment"
                          ).unless(readOnly),
                          <.input(
                            ExploreStyles.FileUpload,
                            ^.tpe    := "file",
                            ^.onChange ==> onInsertFileSelected(
                              props.programId,
                              props.attachments.mod,
                              attType,
                              client,
                              meta.action
                            ),
                            ^.id     := s"attachment-upload-$attType",
                            ^.name   := "file",
                            ^.accept := attType.accept
                          ).unless(readOnly)
                        ),
                      thisAtt =>
                        val attType = thisAtt.attachmentType
                        <.div(
                          // The upload "button" needs to be a label. In order to make
                          // the styling consistent they're all labels.
                          <.label(
                            tableLabelButtonClasses,
                            Icons.Trash,
                            ^.onClick ==> deletePrompt(
                              props.attachments.mod,
                              client,
                              thisAtt
                            )
                          ).withTooltip("Delete attachment").unless(readOnly),
                          <.label(
                            tableLabelButtonClasses,
                            ^.htmlFor := s"attachment-replace-$attType",
                            Icons.FileArrowUp
                          ).withTooltip(
                            tooltip = s"Upload replacement file",
                            placement = Placement.Right
                          ).unless(readOnly),
                          <.input(
                            ExploreStyles.FileUpload,
                            ^.tpe    := "file",
                            ^.onChange ==> onUpdateFileSelected(
                              props.attachments.mod,
                              thisAtt,
                              client,
                              meta.action
                            ),
                            ^.id     := s"attachment-replace-$attType",
                            ^.name   := "file",
                            ^.accept := attType.accept
                          ).unless(readOnly),
                          meta.urlMap
                            .get(thisAtt.toMapKey)
                            .foldMap:
                              case Pot.Ready(url) =>
                                <.a(Icons.FileArrowDown, ^.href := url, tableLabelButtonClasses)
                                  .withTooltip("Download File")
                              case Pot.Pending    => <.span(Icons.Spinner.withSpin(true))
                              case Pot.Error(t)   =>
                                <.span(Icons.ExclamationTriangle).withTooltip(t.getMessage)
                        )
                    )
                ),
              column(AttachmentTypeColumnId, _.attachmentType)
                .withCell(_.value.shortName),
              column(FileNameColumnId, _.fileName),
              column(SizeColumnId, _.fileSize)
                .withCell(cell => cell.value.foldMap(_.toHumanReadableByteCount)),
              column(LastUpdateColumnId, _.updatedAt)
                .withCell(
                  _.value.foldMap(ts => Constants.GppDateFormatter.format(ts.toLocalDateTime))
                )
            )
      // Rows
      .useMemoBy((props, _, _, _, _, _) => (props.attachments.reuseByValue, props.proposalType)):
        (_, _, _, _, _, _) =>
          (vl, pt) =>
            val pas = vl.get.proposalList
            Enumerated[AttachmentType]
              .forPurpose(AttachmentPurpose.Proposal)
              .filterNot(
                // Fast turnaround proposals do not have a team attachment
                _ === AttachmentType.Team && pt.exists(t =>
                  t.scienceSubtype === ScienceSubtype.FastTurnaround
                )
              )
              .map(pat => pas.find(_.attachmentType === pat).toRight(pat))
      .useReactTableBy: (_, _, _, action, urlMap, cols, rows) =>
        TableOptions(
          cols,
          rows,
          enableSorting = false,
          getRowId = (row, _, _) => RowId(row.attachmentType.tag),
          meta = TableMeta(action = action, urlMap = urlMap.get)
        )
      .render { (_, _, _, action, _, _, _, table) =>
        React.Fragment(
          PrimeTable(
            table,
            striped = true,
            compact = Compact.Very,
            emptyMessage = <.div("No proposal attachments uploaded"),
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
}
