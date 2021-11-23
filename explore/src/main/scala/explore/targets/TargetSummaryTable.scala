// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order._
import cats.syntax.all._
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.TargetListGroupQueries._
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.FocusedObs
import explore.model.SelectedPanel
import explore.model.TargetEnvGroup
import explore.model.TargetEnvGroupIdSet
import explore.model.TargetIdSet
import explore.model.reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Target
import lucuma.ui.reusability._
import react.common._
import react.common.implicits._
import react.semanticui.collections.table._
import react.semanticui.modules.checkbox.Checkbox
import react.semanticui.modules.dropdown.DropdownItem
import react.semanticui.modules.dropdown._
import reactST.reactTable._
import reactST.reactTable.mod.DefaultSortTypes
import reactST.reactTable.mod.IdType

import scala.collection.immutable.SortedSet

import scalajs.js.JSConverters._

final case class TargetSummaryTable(
  targetListGroupWithObs: TargetListGroupWithObs,
  hiddenColumns:          View[Set[String]],
  selectedPanel:          View[SelectedPanel[TargetEnvGroupIdSet]],
  focusedObs:             View[Option[FocusedObs]],
  expandedIds:            View[SortedSet[TargetEnvGroupIdSet]],
  renderInTitle:          Tile.RenderInTitle
) extends ReactFnProps[TargetSummaryTable](TargetSummaryTable.component)

final case class TargetRow(
  id:                  String,
  name:                NonEmptyString,
  target:              Option[Target],
  optTargetEnvGroupId: Option[TargetEnvGroupIdSet]
)

object TargetRow {
  def expandableFromTarget(
    id:                  TargetIdSet,
    target:              Target,
    optTargetEnvGroupId: Option[TargetEnvGroupIdSet]
  ): Expandable[TargetRow] =
    Expandable(
      // TODO Implement a more expressive toString in TargetIdSet
      TargetRow(id.toString, target.name, target.some, optTargetEnvGroupId)
    )

  def expandableFromTargetEnv(targetEnv: TargetEnvGroup): Option[Expandable[TargetRow]] =
    targetEnv.scienceTargets.toList match {
      case Nil                              => none
      case (targetIds, singleTarget) :: Nil =>
        expandableFromTarget(targetIds, singleTarget, targetEnv.id.some).some
      case targets                          =>
        Expandable(
          TargetRow(targetEnv.id.toString, targetEnv.name, none, targetEnv.id.some)
        ).withSubRows(targets.map { case (id, target) => expandableFromTarget(id, target, none) })
          .some
    }
}

object TargetSummaryTable {
  type Props = TargetSummaryTable

  implicit protected val propsReuse: Reusability[Props] = Reusability.derive

  protected val TargetTable =
    TableDef[Expandable[TargetRow]].withExpanded.withSortBy

  protected val TargetTableComponent = new SUITable(TargetTable)

  private val columnClasses: Map[String, Css] = Map(
    "expander" -> (ExploreStyles.Sticky |+| ExploreStyles.TargetSummaryExpander),
    "type"     -> (ExploreStyles.Sticky |+| ExploreStyles.TargetSummaryType |+| ExploreStyles.WithExpander),
    "name"     -> (ExploreStyles.Sticky |+| ExploreStyles.TargetSummaryName |+| ExploreStyles.WithExpander)
  )

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      // cols
      .useMemoBy(_ => ()) { props => _ =>
        def column[V](id: String, accessor: TargetRow => V) =
          TargetTable
            .Column(id, row => accessor(row.value))
            .setHeader(TargetColumns.allColNames(id))

        List(
          TargetTable
            .Column("expander")
            .setCell(cell =>
              if (cell.row.canExpand)
                <.span(
                  cell.row.getToggleRowExpandedProps(),
                  if (cell.row.isExpanded.contains(true)) Icons.ChevronDown
                  else
                    <.span(
                      // The next line is temporary. We're just compensanting for the fact that column
                      // widths are being completely ignored for some reason we don't quite comprehend yet.
                      ^.paddingRight := "3px",
                      Icons.ChevronRight
                    )
                )
              else ""
            )
            .setDisableSortBy(true),
          TargetTable
            .Column("type")
            .setCell(cell =>
              <.span(
                ExploreStyles.TargetSummarySubRowCell.when_(cell.row.depth > 0),
                // The next line is temporary. We're just compensanting for the fact that column
                // widths are being completely ignored for some reason we don't quite comprehend yet.
                (^.paddingRight := "15px").when(cell.row.depth == 0),
                if (cell.row.subRows.isEmpty) Icons.Star
                else Icons.Stars
              )
            ),
          column("name", _.name)
            .setCell(cell =>
              <.a(
                ExploreStyles.TargetSummarySubRowCell.when_(cell.row.depth > 0),
                ^.onClick ==> (_ =>
                  cell.row.original.value.optTargetEnvGroupId
                    .map(targetEnvGroupId => // TODO Allow jumping to a specific target?
                      props.expandedIds.mod(_ + targetEnvGroupId) >>
                        props.selectedPanel.set(SelectedPanel.editor(targetEnvGroupId))
                    )
                    .orEmpty
                ),
                cell.value.toString
              )
            )
            .setSortByFn(_.toString)
        ) ++
          TargetColumns
            .NonBaseSiderealColumnBuilder(TargetTable)(_.value.target)
            .allColumns ++
          List(
            column("count", _.optTargetEnvGroupId.map(_.obsIdList.length)) // TODO Right align
              .setCell(_.value.map(_.toString).orEmpty)
              .setSortType(DefaultSortTypes.number),
            column("observations", _.optTargetEnvGroupId)
              .setCell(cell =>
                cell.value
                  .map(targetEnvGroupId =>
                    <.span(
                      targetEnvGroupId.obsIdList
                        .map(obsId =>
                          <.a(
                            ^.onClick ==> (_ =>
                              props.focusedObs.set(FocusedObs(obsId).some) >>
                                props.expandedIds.mod(_ + targetEnvGroupId) >>
                                props.selectedPanel
                                  .set(SelectedPanel.editor(targetEnvGroupId))
                            ),
                            obsId.toString
                          )
                        )
                        .mkReactFragment(", ")
                    )
                  )
                  .getOrElse("")
              )
              .setDisableSortBy(true)
          )
      }
      // rows
      .useMemoBy((props, _) => props.targetListGroupWithObs)((_, _) =>
        _.targetListGroups.values.toList.flatMap(TargetRow.expandableFromTargetEnv)
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
                      .map(col => col: IdType[TargetEnvGroupIdSet])
                      .toJSArray
                  )
              )
          }.reuseCurrying(props.hiddenColumns.get)
        )
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
                    .drop(2)
                    .toTagMod { column =>
                      val colId = column.id.toString
                      DropdownItem()(^.key := colId)(
                        <.div(
                          Checkbox(
                            label = TargetColumns.allColNames(colId),
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
                          unstackable = true,
                          clazz = ExploreStyles.ExploreTable
            )(),
            header = true,
            headerCell = (col: TargetTable.ColumnType) =>
              TableHeaderCell(clazz = columnClasses.get(col.id.toString).orUndefined)(
                ^.textTransform.none,
                ^.whiteSpace.nowrap
              ),
            cell = (cell: TargetTable.CellType[_]) =>
              TableCell(clazz = columnClasses.get(cell.column.id.toString).orUndefined)(
                ^.whiteSpace.nowrap
              )
          )(tableInstance)
        )
      )
}
