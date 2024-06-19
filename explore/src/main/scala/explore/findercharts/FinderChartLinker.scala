// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.findercharts

import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import crystal.react.reuse.*
import explore.Icons
import explore.attachments.Action
import explore.attachments.ObsAttachmentUtils
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ObsAttachment
import explore.model.ObsAttachmentList
import explore.utils.OdbRestClient
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.ObsAttachment as ObsAtt
import lucuma.core.model.Program
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Checkbox
import lucuma.react.primereact.Divider
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import org.scalajs.dom.*

import scala.collection.immutable.SortedSet

case class FinderChartLinker(
  programId:        Program.Id,
  client:           OdbRestClient[IO],
  selected:         View[Option[ObsAtt.Id]],
  obsAttachmentIds: View[SortedSet[ObsAtt.Id]],
  obsAttachments:   ObsAttachmentList
) extends ReactFnProps[FinderChartLinker](FinderChartLinker.component)

object FinderChartLinker extends ObsAttachmentUtils with FinderChartsAttachmentUtils:
  private type Props = FinderChartLinker

  private case class TableMeta(
    selected:         View[Option[ObsAtt.Id]],
    obsAttachmentIds: View[SortedSet[ObsAtt.Id]]
  )

  private val ColDef = ColumnDef.WithTableMeta[ObsAttachment, TableMeta]

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateView(Action.None)
      .useMemoBy((_, _, _) => ()): (_, _, action) =>
        _ =>
          def column[V](id: ColumnId, accessor: ObsAttachment => V)
            : ColumnDef.Single.WithTableMeta[ObsAttachment, V, TableMeta] =
            ColDef(id, v => accessor(v), columnNames(id))

          List(
            column(AttIdColumnId, _.id)
              .setCell: cell =>
                cell.table.options.meta
                  .map: meta =>
                    Checkbox(
                      meta.obsAttachmentIds.get.contains(cell.value),
                      onChangeE = { (e: Boolean, ev: ReactEventFrom[Element]) =>
                        (for {
                          _ <- ev.preventDefaultCB *> ev.stopPropagationCB
                          _ <- action.set(Action.Unlink)
                          _ <- meta.obsAttachmentIds.mod(_ - cell.value).unless_(e)
                          _ <- meta.selected
                                 .set(none)
                                 .when_(e && meta.selected.get.contains(cell.value))
                          // We need to delay setting the selection because the newly added chart
                          // Is not yet on the obs attachment ids
                          // It is a bit dirty but we can't reach the table from here to set the selection
                          _ <-
                            meta.selected.set(Option(cell.value)).delayMs(200).toCallback.when_(e)
                          _ <- meta.obsAttachmentIds.mod(_ + cell.value).when_(e)
                        } yield ()).finallyRun(action.set(Action.None))
                      }
                    )
              .setEnableSorting(false),
            column(FileNameColumnId, ObsAttachment.fileName.get)
              .setCell(_.value.value)
              .sortableBy(_.value.toUpperCase)
          )
      // Rows
      .useMemoBy((props, _, _, _) => (props.obsAttachmentIds.reuseByValue, props.obsAttachments)):
        (_, _, _, _) => (_, obsAttachments) => obsAttachments.map(_._2).toList
      .useReactTableBy: (props, _, _, cols, rows) =>
        TableOptions(
          cols,
          rows,
          enableRowSelection = true,
          getRowId = (row, _, _) => RowId(row.id.show),
          meta = TableMeta(props.selected, props.obsAttachmentIds)
        )
      .useEffectWithDepsBy((p, _, _, _, _, _) => p.selected.reuseByValue): (p, _, _, _, _, table) =>
        _ =>
          val selection =
            p.selected.get
              .map(id => Map[RowId, Boolean](RowId(id.show) -> true))
              .getOrElse(Map.empty)
          table.setRowSelection(RowSelection(selection))
      .render: (p, ctx, action, _, _, table) =>
        <.div(
          ExploreStyles.FinderChartsAttachments,
          ^.onClick ==> { e =>
            e.stopPropagationCB
          },
          <.span(
            Icons.Files.withFixedWidth(true),
            "Available charts"
          ),
          Divider(),
          PrimeAutoHeightVirtualizedTable(
            table,
            _ => 32.toPx,
            striped = true,
            compact = Compact.Very,
            emptyMessage = "No charts",
            innerContainerMod = ^.width := "100%",
            headerMod = ExploreStyles.FinderChartsTableHeader,
            tableMod =
              ExploreStyles.FinderChartsTable |+| ExploreStyles.ExploreSelectableTable |+| ExploreStyles.ExploreTable |+| ExploreStyles.FinderChartsTableDisabled
                .unless_(action.get === Action.None),
            rowMod = row =>
              TagMod(
                ExploreStyles.TableRowSelected.when_(row.getIsSelected()),
                ^.onClick ==> { e =>
                  val selectedId =
                    ObsAtt.Id.parse(row.id.value).filter(p.obsAttachmentIds.get.contains)
                  for {
                    _ <- e.preventDefaultCB *> e.stopPropagationCB
                    _ <- p.selected.set(selectedId)
                    _ <- table.toggleAllRowsSelected(false)
                    _ <- row.toggleSelected()
                  } yield ()
                }
              )
          )
        )
