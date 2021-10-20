// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.constraints

import cats.Order._
import cats.data.NonEmptySet
import cats.syntax.all._
import crystal.react.implicits._
import crystal.react.reuse._
import explore.Icons
import explore.common.ConstraintGroupQueries._
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.AirMassRange
import explore.model.ConstraintGroup
import explore.model.ConstraintSet
import explore.model.Focused
import explore.model.Focused._
import explore.model.HourAngleRange
import explore.model.ObsIdSet
import explore.model.SelectedPanel
import explore.model.reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.reusability._
import react.common._
import react.common.implicits._
import react.semanticui.collections.table._
import react.semanticui.modules.checkbox.Checkbox
import react.semanticui.modules.dropdown.DropdownItem
import react.semanticui.modules.dropdown._
import reactST.reactTable._
import reactST.reactTable.mod.Cell
import reactST.reactTable.mod.DefaultSortTypes
import reactST.reactTable.mod.IdType
import reactST.reactTable.mod.SortingRule
import reactST.reactTable.mod.TableState

import scala.collection.immutable.SortedSet

import scalajs.js.JSConverters._

final case class ConstraintsSummaryTable(
  constraintList: ConstraintGroupList,
  hiddenColumns:  View[Set[String]],
  summarySorting: View[List[(String, Boolean)]],
  selectedPanel:  View[SelectedPanel[ObsIdSet]],
  focused:        View[Option[Focused]],
  expandedIds:    View[SortedSet[ObsIdSet]],
  renderInTitle:  Tile.RenderInTitle
) extends ReactFnProps[ConstraintsSummaryTable](ConstraintsSummaryTable.component)

object ConstraintsSummaryTable {
  type Props = ConstraintsSummaryTable

  protected val ConstraintsTable = TableDef[ConstraintGroup].withSort

  import ConstraintsTable.syntax._

  protected val ConstraintsTableComponent = new SUITable(ConstraintsTable)

  implicit protected val reuseProps: Reusability[Props] = Reusability.derive

  private val columnNames: Map[String, String] = Map(
    "edit"         -> " ",
    "iq"           -> "IQ",
    "cc"           -> "CC",
    "bg"           -> "BG",
    "wv"           -> "WV",
    "minam"        -> "Min AM",
    "maxam"        -> "Max AM",
    "minha"        -> "Min HA",
    "maxha"        -> "Max HA",
    "count"        -> "Count",
    "observations" -> "Observations"
  )

  private val columnClasses: Map[String, Css] = Map(
    "edit" -> (ExploreStyles.Sticky |+| ExploreStyles.ConstraintsSummaryEdit)
  )

  private def toSortingRules(tuples: List[(String, Boolean)]) = tuples.map { case (id, b) =>
    SortingRule[ConstraintGroup](id).setDesc(b)
  }.toJSArray

  private def fromTableState(state: TableState[ConstraintGroup]): List[(String, Boolean)] = state
    .asInstanceOf[ConstraintsTable.StateType]
    .sortBy
    .toList
    .map(sr => (sr.id.toString, sr.desc.toOption.getOrElse(false)))

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useMemoBy(_ => ()) { props => _ => // Cols never changes, but needs access to props
        def column[V](id: String, accessor: ConstraintGroup => V) =
          ConstraintsTable
            .Column(id, accessor)
            .setHeader(columnNames(id))

        List(
          column("edit", ConstraintGroup.obsIds.get)
            .setCell(cell =>
              <.a(^.onClick ==> (_ => props.selectedPanel.set(SelectedPanel.editor(cell.value))),
                  Icons.Edit
              )
            )
            .setDisableSortBy(true),
          column("iq", ConstraintGroup.constraintSet.andThen(ConstraintSet.imageQuality).get)
            .setCell(_.value.label)
            .setSortByFn(_.label),
          column("cc", ConstraintGroup.constraintSet.andThen(ConstraintSet.cloudExtinction).get)
            .setCell(_.value.label)
            .setSortByFn(_.label),
          column("bg", ConstraintGroup.constraintSet.andThen(ConstraintSet.skyBackground).get)
            .setCell(_.value.label)
            .setSortByFn(_.label),
          column("wv", ConstraintGroup.constraintSet.andThen(ConstraintSet.waterVapor).get)
            .setCell(_.value.label)
            .setSortByFn(_.label),
          column("minam", ConstraintGroup.constraintSet.andThen(ConstraintSet.elevationRange).get)
            .setCell(_.value match {
              case AirMassRange(min, _) => f"${min.value}%.1f"
              case HourAngleRange(_, _) => ""
            })
            .setSortByFn(_ match {
              case AirMassRange(min, _) => min.value
              case HourAngleRange(_, _) => AirMassRange.MinValue - 1
            }),
          column("maxam", ConstraintGroup.constraintSet.andThen(ConstraintSet.elevationRange).get)
            .setCell(_.value match {
              case AirMassRange(_, max) => f"${max.value}%.1f"
              case HourAngleRange(_, _) => ""
            })
            .setSortByFn(_ match {
              case AirMassRange(_, max) => max.value
              case HourAngleRange(_, _) => AirMassRange.MinValue - 1
            }),
          column("minha", ConstraintGroup.constraintSet.andThen(ConstraintSet.elevationRange).get)
            .setCell(_.value match {
              case AirMassRange(_, _)     => ""
              case HourAngleRange(min, _) => f"${min.value}%.1f"
            })
            .setSortByFn(_ match {
              case AirMassRange(_, _)     => HourAngleRange.MinHour - 1
              case HourAngleRange(min, _) => min.value
            }),
          column("maxha", ConstraintGroup.constraintSet.andThen(ConstraintSet.elevationRange).get)
            .setCell(_.value match {
              case AirMassRange(_, _)     => ""
              case HourAngleRange(_, max) => f"${max.value}%.1f"
            })
            .setSortByFn(_ match {
              case AirMassRange(_, _)     => HourAngleRange.MinHour - 1
              case HourAngleRange(_, max) => max.value
            }),
          column("count", _.obsIds.length)
            .setSortType(DefaultSortTypes.number),
          column("observations", ConstraintGroup.obsIds.get)
            .setCell(cell =>
              <.span(
                cell.value.toSortedSet.toList
                  .map(obsId =>
                    <.a(
                      ^.onClick ==> (_ =>
                        (props.focused.set(FocusedObs(obsId).some)
                          >> props.expandedIds.mod(_ + cell.value)
                          >> props.selectedPanel.set(SelectedPanel.editor(NonEmptySet.one(obsId))))
                      ),
                      obsId.toString
                    )
                  )
                  .mkReactFragment(", ")
              )
            )
            .setDisableSortBy(true)
        )
      }
      .useMemoBy((props, _) => props.constraintList)((_, _) => _.values.toList) // Memo rows
      .useTableBy((props, cols, rows) =>
        ConstraintsTable(
          cols,
          rows,
          { (hiddenColumns: Set[String], options: ConstraintsTable.OptionsType) =>
            options
              .setAutoResetSortBy(false)
              .setInitialStateFull(
                ConstraintsTable
                  .State()
                  .setHiddenColumns(
                    hiddenColumns.toList.map(col => col: IdType[ConstraintGroup]).toJSArray
                  )
                  .setSortBy(toSortingRules(props.summarySorting.get))
              )
          }.reuseCurrying(props.hiddenColumns.get)
        )
      )
      .useEffectWithDepsBy((_, _, _, tableInstance) => fromTableState(tableInstance.state))(
        (props, _, _, _) => rules => props.summarySorting.set(rules)
      )
      .render((props, _, _, tableInstance) =>
        <.div(
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
                    .drop(1)
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
          ConstraintsTableComponent(
            table = Table(celled = true,
                          selectable = true,
                          striped = true,
                          compact = TableCompact.Very
            )(),
            header = true,
            headerCell = (col: ConstraintsTable.ColumnType) =>
              TableHeaderCell(clazz = columnClasses.get(col.id.toString).orUndefined)(
                ^.textTransform.none,
                ^.whiteSpace.nowrap
              ),
            cell = (cell: Cell[ConstraintGroup, _]) =>
              TableCell(clazz = columnClasses.get(cell.column.id.toString).orUndefined)(
                ^.whiteSpace.nowrap
              )
          )(tableInstance)
        )
      )
}
