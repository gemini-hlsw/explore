package explore.targeteditor

import cats.syntax.all._
import explore.implicits._
import lucuma.core.model.User
import explore.model.TargetEnv
import lucuma.core.model.Target
import explore.undo.UndoStacks
import explore.model.TargetVisualOptions
import cats.effect.IO
import lucuma.core.model.SiderealTarget
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import explore.model.ScienceTarget
import reactST.reactTable.TableDef
import reactST.reactTable.SUITable
import explore.model.reusability._
import crystal.react.implicits._
import lucuma.core.enum.MagnitudeBand
import react.common.style.Css
import explore.components.ui.ExploreStyles
import explore.Icons

final case class TargetTable(
  targets: List[ScienceTarget]
  // undoStacks: View[Map[Target.Id, UndoStacks[IO, SiderealTarget]]],
)

object TargetTable {
  type Props = TargetTable

  protected val TargetTable = TableDef[ScienceTarget].withSort

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
    "rv"           -> "RV",
    "z"            -> "z",
    "cz"           -> "cz",
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
      // cols
      .useMemo(()) { _ =>
        def column[V](id: String, accessor: ScienceTarget => V) =
          TargetTable
            .Column(id, accessor)
            .setHeader(columnNames(id))

        List(
          column("type", _ => ())
            .setCell(_ => Icons.Star)
            .setWidth(30),
          column("name", ScienceTarget.name.get)
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
            column("rv", TargetObsQueries.rvLens.get)
              .setCell(_.value.map(formatRV.reverseGet).orEmpty)
              .setSortByAuto,
            column("z", (TargetObsQueries.rvLens.get _).andThen(rvToRedshiftGet))
              .setCell(_.value.map(formatZ.reverseGet).orEmpty)
              .setSortByAuto,
            column("cz", (TargetObsQueries.rvLens.get _).andThen(rvToARVGet))
              .setCell(_.value.map(formatCZ.reverseGet).orEmpty)
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
