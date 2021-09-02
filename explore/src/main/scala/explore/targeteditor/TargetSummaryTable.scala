// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Order._
import cats.syntax.all._
import crystal.react.implicits._
import crystal.react.reuse._
import explore.Icons
import explore.common.TargetObsQueries
import explore.common.TargetObsQueries._
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.ExpandedIds
import explore.model.Focused
import explore.model.formats._
import explore.model.reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.MagnitudeBand
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.MagnitudeValue
import lucuma.core.math.Parallax
import lucuma.core.model.Magnitude
import lucuma.core.model.Target
import lucuma.ui.optics.TruncatedDec
import lucuma.ui.optics.TruncatedRA
import lucuma.ui.optics.ValidFormatInput
import react.common._
import react.common.implicits._
import react.semanticui.collections.table._
import react.semanticui.modules.checkbox.Checkbox
import react.semanticui.modules.dropdown.DropdownItem
import react.semanticui.modules.dropdown._
import reactST.reactTable.implicits._
import reactST.reactTable._
import reactST.reactTable.mod.Cell
import reactST.reactTable.mod.DefaultSortTypes
import reactST.reactTable.mod.IdType

import scalajs.js.JSConverters._

final case class TargetSummaryTable(
  pointingsWithObs: PointingsWithObs,
  hiddenColumns:    View[Set[String]],
  focused:          View[Option[Focused]],
  expandedIds:      View[ExpandedIds],
  renderInTitle:    Tile.RenderInTitle
) //extends ReactProps[TargetSummaryTable](TargetSummaryTable.component)

object TargetSummaryTable {
  type Props = TargetSummaryTable

  implicit def render(props: Props): VdomElement = component(props).vdomElement

  protected val TargetTable = TableDef[TargetResult].withSort

  import TargetTable.syntax._

  protected val TargetTableComponent = new SUITable(TargetTable)

  implicit protected val propsReuse: Reusability[Props] = Reusability.derive

  private val columnNames: Map[String, String] = Map(
    "type"         -> " ",
    "name"         -> "Name",
    "ra"           -> "RA",
    "dec"          -> "Dec",
    "priority"     -> "Priority",
    "count"        -> "Count",
    "observations" -> "Observations",
    "epoch"        -> "Epoch",
    "pmra"         -> "µ RA",
    "pmdec"        -> "µ Dec",
    "parallax"     -> "Parallax",
    "morphology"   -> "Morphology",
    "sed"          -> "SED"
  ) ++ MagnitudeBand.all.map(m => (m.shortName + "mag", m.shortName + "Mag")).toMap

  private val columnClasses: Map[String, Css] = Map(
    "type" -> (ExploreStyles.Sticky |+| ExploreStyles.TargetSummaryType),
    "name" -> (ExploreStyles.Sticky |+| ExploreStyles.TargetSummaryName)
  )

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      .useMemoBy(_.pointingsWithObs.observations) { props => observations => // Memo cols
        def targetObservations(id: Target.Id): List[ObsResult] =
          observations.toList.filter(_.pointing match {
            case Some(PointingTargetResult(tid)) => tid === id
            case _                               => false
          })

        def column[V](id: String, accessor: TargetResult => V) =
          TargetTable
            .Column(id, accessor)
            .setHeader(columnNames(id))

        List(
          column("type", _ => ())
            .setCell(_ => Icons.Star)
            .setWidth(30),
          column("name", TargetResult.name.get)
            .setCell(cell =>
              <.a(^.onClick ==> (_ =>
                    props.focused.set(Focused.FocusedTarget(cell.row.original.id).some)
                  ),
                  cell.value.toString
              )
            )
            .setSortByFn(_.toString),
          column(
            "ra",
            TargetObsQueries.baseCoordinatesRa.get
          ).setCell(cell =>
            TruncatedRA.rightAscension.get
              .andThen(ValidFormatInput.truncatedRA.reverseGet)(cell.value)
          ).setSortByAuto,
          column[Declination](
            "dec",
            TargetObsQueries.baseCoordinatesDec.get
          ).setCell(cell =>
            TruncatedDec.declination.get
              .andThen(ValidFormatInput.truncatedDec.reverseGet)(cell.value)
          ).setSortByAuto,
          column("priority", _ => "")
        ) ++
          MagnitudeBand.all.map(m =>
            column(
              m.shortName + "mag",
              _.magnitudes.collectFirst {
                case Magnitude(value, band, _, _) if band === m => value
              }
            ).setCell(_.value.map(MagnitudeValue.fromString.reverseGet).orEmpty).setSortByAuto
          ) ++
          List(
            column("epoch", TargetObsQueries.epoch.get)
              .setCell(cell =>
                s"${cell.value.scheme.prefix}${Epoch.fromStringNoScheme.reverseGet(cell.value)}"
              )
              .setSortByAuto,
            column("pmra", TargetObsQueries.pmRALens.get)
              .setCell(
                _.value.map(pmRAFormat.reverseGet).orEmpty
              )
              .setSortByAuto,
            column("pmdec", TargetObsQueries.pmDecLens.get)
              .setCell(_.value.map(pmDecFormat.reverseGet).orEmpty)
              .setSortByAuto,
            column("parallax", TargetObsQueries.pxLens.get)
              .setCell(_.value.map(Parallax.milliarcseconds.get).map(_.toString).orEmpty)
              .setSortByAuto,
            column("morphology", _ => ""),
            column("sed", _ => ""),
            column(
              "count",
              target => targetObservations(target.id).length
            ).setSortType(DefaultSortTypes.number),
            column("observations", target => targetObservations(target.id))
              .setCell(cell =>
                <.span(
                  cell.value
                    .map(obs =>
                      <.a(
                        ^.onClick ==> (_ =>
                          (props.focused
                            .set(Focused.FocusedObs(obs.id).some) >> props.expandedIds
                            .mod(ExpandedIds.targetIds.modify(_ + cell.row.original.id)))
                        ),
                        obs.id.toString()
                      )
                    )
                    .mkReactFragment(", ")
                )
              )
              .setDisableSortBy(true)
          )
      }
      .useMemoBy((props, _) => props.pointingsWithObs)((_, _) => _.targets.toList) // Memo rows
      .useTableBy((props, cols, rows) =>
        TargetTable(
          cols,
          rows,
          { (hiddenColumns: Set[String], options: TargetTable.OptionsType) =>
            options
              .setAutoResetSortBy(false)
              .setInitialStateFull(
                TargetTable
                  .State()
                  .setHiddenColumns(
                    hiddenColumns.toList
                      .map(col => col: IdType[TargetResult])
                      .toJSArray
                  )
              )
          }.reuseCurrying(props.hiddenColumns.get)
        )
      )
      .render((props, _, _, tableInstance) =>
        <.div(
          props.renderInTitle(
            <.span(ExploreStyles.TitleStrip)(
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
                          compact = TableCompact.Very
            )(),
            header = true,
            headerCell = (col: TargetTable.ColumnType) =>
              TableHeaderCell(clazz = columnClasses.get(col.id.toString).orUndefined)(
                ^.textTransform.none,
                ^.whiteSpace.nowrap
              ),
            cell = (cell: Cell[TargetResult, _]) =>
              TableCell(clazz = columnClasses.get(cell.column.id.toString).orUndefined)(
                ^.whiteSpace.nowrap
              )
          )(tableInstance)
        )
      )
}
