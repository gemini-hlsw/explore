// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all._
import crystal.Pot
import crystal.react.View
import crystal.react.hooks._
import crystal.react.implicits._
import crystal.react.reuse._
import explore.Icons
import explore.common.AsterismQueries
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.Asterism
import explore.model.ObsIdSet
import explore.model.SiderealTargetWithId
import explore.model.reusability._
import explore.targets.TargetColumns
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Target
import lucuma.ui.reusability._
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.Css
import react.common.ReactFnProps
import react.semanticui.collections.table._
import react.semanticui.elements.button._
import react.semanticui.modules.checkbox.Checkbox
import react.semanticui.modules.dropdown.DropdownItem
import react.semanticui.modules.dropdown._
import react.semanticui.shorthand._
import react.semanticui.sizes._
import reactST.reactTable.SUITable
import reactST.reactTable.TableDef
import reactST.reactTable._
import reactST.reactTable.mod.IdType

import java.time.Instant

import scalajs.js.JSConverters._

final case class TargetTable(
  obsIds:           ObsIdSet,
  targets:          View[Option[Asterism]],
  hiddenColumns:    View[Set[String]],
  selectedTarget:   View[Option[Target.Id]],
  vizTime:          Option[Instant],
  renderInTitle:    Tile.RenderInTitle,
  fullScreen:       Boolean
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[TargetTable](TargetTable.component)

object TargetTable {
  type Props = TargetTable

  protected val TargetTable = TableDef[SiderealTargetWithId].withSortBy

  protected val TargetTableComponent = new SUITable(TargetTable)

  private val columnNames: Map[String, String] = Map(
    "delete" -> " "
  ) ++ TargetColumns.allColNames

  private val columnClasses: Map[String, Css] = Map(
    "delete" -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryDelete),
    "type"   -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryType |+| ExploreStyles.WithDelete),
    "name"   -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryName |+| ExploreStyles.WithDelete)
  )

  private def deleteSiderealTarget(
    obsIds:       ObsIdSet,
    targetId:     Target.Id
  )(implicit ctx: AppContextIO): IO[Unit] =
    AsterismQueries.removeTargetFromAsterisms[IO](obsIds.toList, targetId)

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      // cols
      .useMemoBy(props => (props.obsIds, props.targets.get)) { props => _ =>
        implicit val ctx = props.ctx

        def column[V](id: String, accessor: SiderealTargetWithId => V) =
          TargetTable
            .Column(id, accessor)
            .setHeader(columnNames(id))

        List(
          column("delete", _.id)
            .setCell(cell =>
              Button(
                size = Tiny,
                compact = true,
                clazz = ExploreStyles.DeleteButton |+| ExploreStyles.ObsDeleteButton,
                icon = Icons.Trash,
                onClickE = (e: ReactMouseEvent, _: Button.ButtonProps) =>
                  e.preventDefaultCB >>
                    e.stopPropagationCB >>
                    props.targets.mod(_.flatMap(_.remove(cell.value.extract))) >>
                    deleteSiderealTarget(props.obsIds, cell.value).runAsync
              )
            )
            .setDisableSortBy(true)
        ) ++
          TargetColumns
            .BaseColumnBuilder(TargetTable)(_.target.some)
            .allColumns
      }
      // If vizTime is not set, change it to now
      .useEffectResultWithDepsBy((p, _) => p.vizTime) { (_, _) => vizTime =>
        IO(vizTime.getOrElse(Instant.now()))
      }
      // rows
      .useMemoBy((props, _, vizTime) => (props.targets.get, vizTime))((_, _, _) => {
        case (targets, Pot.Ready(vizTime)) => targets.foldMap(_.toSidereal(vizTime))
        case _                             => Nil
      })
      .useTableBy((props, cols, _, rows) =>
        TargetTable(
          cols,
          rows,
          { (hiddenColumns: Set[String], options: TargetTable.OptionsType) =>
            options
              .setAutoResetSortBy(false)
              .setInitialState(
                TargetTable
                  .State()
                  .setHiddenColumns(
                    hiddenColumns.toList
                      .map(col => col: IdType[SiderealTargetWithId])
                      .toJSArray
                  )
              )
          }.reuseCurrying(props.hiddenColumns.get)
        )
      )
      .render((props, _, _, rows, tableInstance) =>
        React.Fragment(
          props.renderInTitle(
            <.span(ExploreStyles.TitleSelectColumns)(
              Dropdown(item = true,
                       simple = true,
                       pointing = Pointing.TopRight,
                       scrolling = true,
                       text = "Columns",
                       clazz = ExploreStyles.SelectColumns
              )(
                DropdownMenu(
                  tableInstance.allColumns
                    .drop(2)
                    .toTagMod { column =>
                      val colId = column.id.toString
                      DropdownItem()(^.key := colId)(
                        <.div(
                          Checkbox(
                            label = columnNames(colId),
                            checked = column.isVisible,
                            onChange = (value: Boolean) =>
                              Callback(column.toggleHidden()) >>
                                props.hiddenColumns
                                  .mod(cols => if (value) cols - colId else cols + colId)
                          )
                        )
                      )
                    }
                )
              ).unless(props.fullScreen)
            )
          ),
          if (rows.isEmpty) {
            <.div(
              ExploreStyles.FullHeightWidth |+| ExploreStyles.HVCenter |+| ExploreStyles.EmptyTreeContent,
              <.div("Add a target")
            )
          } else {
            <.div(ExploreStyles.ExploreTable |+| ExploreStyles.AsterismTable)(
              TargetTableComponent(
                table = Table(
                  celled = true,
                  selectable = true,
                  striped = true,
                  compact = TableCompact.Very,
                  unstackable = true
                )(),
                header = true,
                headerCell = (col: TargetTable.ColumnType) =>
                  TableHeaderCell(clazz =
                    columnClasses.get(col.id.toString).orEmpty |+| ExploreStyles.StickyHeader
                  )(
                    ^.textTransform.none,
                    ^.whiteSpace.nowrap
                  ),
                row = (rowData: TargetTable.RowType) =>
                  TableRow(
                    clazz = ExploreStyles.TableRowSelected.when_(
                      props.selectedTarget.get.exists(_ === rowData.original.id)
                    )
                  )(
                    ^.onClick --> props.selectedTarget
                      .set(rowData.original.id.some),
                    props2Attrs(rowData.getRowProps())
                  ),
                cell = (cell: TargetTable.CellType[?]) =>
                  TableCell(clazz = columnClasses.get(cell.column.id.toString).orUndefined)(
                    ^.whiteSpace.nowrap
                  )
              )(tableInstance)
            )
          }
        )
      )
}
