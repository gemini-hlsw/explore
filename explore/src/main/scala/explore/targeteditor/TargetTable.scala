// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import crystal.react.reuse._
import explore.Icons
import explore.common.TargetEnvQueriesGQL
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.SiderealTargetWithId
import explore.model.TargetIdSet
import explore.model.TargetWithId
import explore.model.reusability._
import explore.targets.TargetColumns
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.SiderealTarget
import lucuma.core.model.Target
import react.common._
import react.common.implicits._
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

import scala.collection.immutable.TreeSeqMap

import scalajs.js.JSConverters._

final case class TargetTable(
  targets:          View[TreeSeqMap[TargetIdSet, Target]],
  hiddenColumns:    View[Set[String]],
  selectedTarget:   View[Option[TargetIdSet]],
  renderInTitle:    Tile.RenderInTitle
  // undoStacks: View[Map[Target.Id, UndoStacks[IO, SiderealTarget]]],
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[TargetTable](TargetTable.component)

object TargetTable {
  type Props = TargetTable

  implicit protected val propsReuse: Reusability[Props] = Reusability.derive

  protected val TargetTable = TableDef[SiderealTargetWithId].withSortBy

  protected val TargetTableComponent = new SUITable(TargetTable)

  private val columnNames: Map[String, String] = Map(
    "delete" -> " "
  ) ++ TargetColumns.allColNames

  private val columnClasses: Map[String, Css] = Map(
    "type" -> (ExploreStyles.Sticky |+| ExploreStyles.TargetSummaryType),
    "name" -> (ExploreStyles.Sticky |+| ExploreStyles.TargetSummaryName)
  )

  private def deleteSiderealTarget(
    targetId:     TargetIdSet
  )(implicit ctx: AppContextIO): IO[Unit] =
    TargetEnvQueriesGQL.RemoveSiderealTarget
      .execute(targetId.toList)
      .void

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      // cols
      .useMemoBy(_ => ()) { props => _ =>
        implicit val ctx = props.ctx

        def column[V](id: String, accessor: SiderealTargetWithId => V) =
          TargetTable
            .Column(id, accessor)
            .setHeader(columnNames(id))

        List(
          column("delete", TargetWithId.id.get)
            .setCell(cell =>
              Button(
                size = Tiny,
                compact = true,
                clazz = ExploreStyles.DeleteButton |+| ExploreStyles.ObsDeleteButton,
                icon = Icons.Trash,
                onClickE = (e: ReactMouseEvent, _: Button.ButtonProps) =>
                  e.preventDefaultCB >>
                    e.stopPropagationCB >>
                    props.targets.mod(_ - cell.value) >>
                    deleteSiderealTarget(cell.value).runAsyncAndForget
              )
            )
            .setWidth(30)
            .setDisableSortBy(true)
        ) ++
          TargetColumns
            .BaseColumnBuilder(TargetTable)((TargetWithId.target.get _).andThen(_.some))
            .allColumns
      }
      // rows
      .useMemoBy((props, _) => props.targets)((_, _) =>
        _.get.collect { case (id, st @ SiderealTarget(_, _, _)) => id -> st }.toList
      )
      .useTableBy((props, cols, rows) =>
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
      .render((props, _, _, tableInstance) =>
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
                DropdownMenu()(
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
              )
            )
          ),
          TargetTableComponent(
            table = Table(celled = true,
                          selectable = true,
                          striped = true,
                          compact = TableCompact.Very,
                          clazz = ExploreStyles.ExploreTable
            )(),
            header = true,
            headerCell = (col: TargetTable.ColumnType) =>
              TableHeaderCell(clazz = columnClasses.get(col.id.toString).orUndefined)(
                ^.textTransform.none,
                ^.whiteSpace.nowrap
              ),
            row = (rowData: TargetTable.RowType) =>
              TableRow(
                clazz = ExploreStyles.TableRowSelected.when_(
                  props.selectedTarget.get.exists(_ === TargetWithId.id.get(rowData.original))
                )
              )(
                ^.onClick --> props.selectedTarget
                  .set(TargetWithId.id.get(rowData.original).some),
                props2Attrs(rowData.getRowProps())
              ),
            cell = (cell: TargetTable.CellType[_]) =>
              TableCell(clazz = columnClasses.get(cell.column.id.toString).orUndefined)(
                ^.whiteSpace.nowrap
              )
          )(tableInstance)
        )
      )
}
