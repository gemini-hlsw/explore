// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.findercharts

import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import crystal.react.reuse.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.attachments.AttachmentType
import explore.attachments.ObsAttachmentUtils
import explore.components.SolarProgress
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ObsAttachment
import explore.model.ObsAttachmentList
import explore.utils.OdbRestClient
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.model.{ObsAttachment => ObsAtt}
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.schemas.ObservationDB.Enums.ObsAttachmentType
import lucuma.ui.primereact.LucumaStyles
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import lucuma.ui.utils.*
import react.common.ReactFnProps
import react.floatingui.Placement
import react.floatingui.syntax.*
import react.primereact.Divider
import react.primereact.PrimeStyles
import react.resizeDetector.hooks.*

import scala.collection.immutable.SortedSet

case class AttachmentsOverlay(
  programId:          Program.Id,
  client:             OdbRestClient[IO],
  selectedAttachment: View[Option[ObsAtt.Id]],
  obsAttachmentIds:   View[SortedSet[ObsAtt.Id]],
  obsAttachments:     View[ObsAttachmentList]
) extends ReactFnProps[AttachmentsOverlay](AttachmentsOverlay.component)

object AttachmentsOverlay extends ObsAttachmentUtils with FinderChartsAttachmentUtils {
  private type Props = AttachmentsOverlay

  private val columnNames: Map[ColumnId, String] = Map(
    AttIdColumnId    -> "ID",
    FileNameColumnId -> "File"
  )

  private val ColDef = ColumnDef[ObsAttachment]

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateView(Action.None)
      .useMemoBy((props, _, _) => ())((p, _, action) =>
        _ =>

          def column[V](id: ColumnId, accessor: ObsAttachment => V)
            : ColumnDef.Single[ObsAttachment, V] =
            ColDef(id, v => accessor(v), columnNames(id))

          List(
            column(AttIdColumnId, _.id)
              .setCell(cell =>
                <.label(
                  Icons.LinkSlash.withClass(ExploreStyles.TrashIcon),
                  ^.onClick ==> { (e: ReactEvent) =>
                    for {
                      _ <- e.preventDefaultCB
                      _ <- action.set(Action.Unlink)
                      _ <- p.obsAttachmentIds.mod(_ - cell.value)
                      _ <- action.set(Action.None)
                    } yield ()
                  }
                ).withTooltip("Unlink from observation", Placement.Right)
              )
              .setEnableSorting(false),
            column(FileNameColumnId, ObsAttachment.fileName.get)
              .setCell(_.value.value)
              .sortableBy(_.value.toUpperCase)
          )
      )
      // Rows
      .useMemoBy((props, _, _, _) =>
        (props.obsAttachmentIds.reuseByValue, props.obsAttachments.reuseByValue)
      )((_, _, _, _) =>
        (obsAttachmentIds, obsAttachments) =>
          validAttachments(obsAttachments.get, obsAttachmentIds.get).map(_._2).toList
      )
      .useReactTableBy((prop, _, _, cols, rows) =>
        TableOptions(
          cols,
          rows,
          getRowId = (row, _, _) => RowId(row.id.show)
        )
      )
      // Propagate the selected attachment
      .useEffectWithDepsBy((_, _, _, _, _, table) =>
        table
          .getSelectedRowModel()
          .rows
          .headOption
          .map(_.getValue[ObsAtt.Id](AttIdColumnId.value))
      )((props, _, _, _, _, _) => selected => props.selectedAttachment.set(selected))
      .useResizeDetector()
      .render { (p, ctx, action, _, _, table, resizer) =>
        import ctx.given

        def addNewFinderChart(e: ReactEventFromInput) =
          onInsertFileSelected(
            p.programId,
            p.obsAttachments,
            AttachmentType.Finder,
            p.client,
            action,
            id =>
              p.obsAttachmentIds.mod(_ + id) *> p.selectedAttachment.set(Some(id)) *> table
                .setRowSelection(RowSelection(Map(RowId(id.show) -> true)))
          )(e)

        ReactFragment(
          <.div(
            ExploreStyles.FinderChartsAttachments,
            SolarProgress(ExploreStyles.FinderChartsTableProgress)
              .unless(action.get === Action.None),
            <.span(
              Icons.Files.withFixedWidth(true),
              "Attachments",
              <.label(
                PrimeStyles.ButtonText |+| ExploreStyles.FinderChartsButton,
                ^.htmlFor := "attachment-upload",
                Icons.FileArrowUp.withBorder(true).withFixedWidth(true)
              ).withTooltip(
                tooltip = s"Upload new finder chart",
                placement = Placement.Right
              ),
              <.input(
                ExploreStyles.FileUpload,
                ^.tpe    := "file",
                ^.onChange ==> addNewFinderChart,
                ^.id     := "attachment-upload",
                ^.name   := "file",
                ^.accept := AttachmentType.Finder.accept
              )
            ),
            Divider(),
            PrimeAutoHeightVirtualizedTable(
              table,
              _ => 32.toPx,
              striped = true,
              compact = Compact.Very,
              emptyMessage = "No charts",
              containerRef = resizer.ref,
              innerContainerMod = ^.width := "100%",
              headerMod = ExploreStyles.FinderChartsTableHeader,
              tableMod =
                ExploreStyles.FinderChartsTable |+| ExploreStyles.ExploreTable |+| ExploreStyles.ExploreSelectableTable |+| ExploreStyles.FinderChartsTableDisabled
                  .unless_(action.get === Action.None),
              rowMod = row =>
                TagMod(
                  ExploreStyles.TableRowSelected.when_(row.getIsSelected()),
                  ^.onClick -->
                    (table.toggleAllRowsSelected(false) >> Callback(row.toggleSelected()))
                )
            )
          )
        )
      }
}
