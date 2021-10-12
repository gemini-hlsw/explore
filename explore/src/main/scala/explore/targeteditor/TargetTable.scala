package explore.targeteditor

import lucuma.core.model.SiderealTarget
import explore.model.ScienceTarget
import reactST.reactTable.TableDef
import reactST.reactTable.SUITable

import cats.Order._
import cats.syntax.all._
import crystal.react.implicits._
import crystal.react.reuse._
import explore.Icons
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.conversions._
import explore.model.formats._
import explore.model.reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.MagnitudeBand
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.MagnitudeValue
import lucuma.core.math.Parallax
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
import reactST.reactTable._
import reactST.reactTable.mod.Cell
import reactST.reactTable.mod.IdType

import scalajs.js.JSConverters._

final case class TargetTable(
  targets:       List[ScienceTarget[Target]],
  hiddenColumns: View[Set[String]],
  renderInTitle: Tile.RenderInTitle
  // undoStacks: View[Map[Target.Id, UndoStacks[IO, SiderealTarget]]],
) extends ReactFnProps[TargetTable](TargetTable.component)

object TargetTable {
  type Props = TargetTable

  protected val TargetTable = TableDef[ScienceTarget[SiderealTarget]].withSort

  import TargetTable.syntax._

  protected val TargetTableComponent = new SUITable(TargetTable)

  implicitly[Reusability[ScienceTarget[Target]]]

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
        def column[V](id: String, accessor: ScienceTarget[SiderealTarget] => V) =
          TargetTable
            .Column(id, accessor)
            .setHeader(columnNames(id))

        List(
          column("type", _ => ())
            .setCell(_ => Icons.Star)
            .setWidth(30),
          column("name", ScienceTarget.name.get)
            .setCell(cell => cell.value.toString)
            .setSortByFn(_.toString),
          column(
            "ra",
            ScienceTarget.Sidereal.baseRA.get
          ).setCell(cell =>
            TruncatedRA.rightAscension.get
              .andThen(ValidFormatInput.truncatedRA.reverseGet)(cell.value)
          ).setSortByAuto,
          column[Declination](
            "dec",
            ScienceTarget.Sidereal.baseDec.get
          ).setCell(cell =>
            TruncatedDec.declination.get
              .andThen(ValidFormatInput.truncatedDec.reverseGet)(cell.value)
          ).setSortByAuto,
          column("priority", _ => "")
        ) ++
          MagnitudeBand.all.map(band =>
            column(
              band.shortName + "mag",
              _.target.magnitudes.get(band).map(_.value)
            ).setCell(_.value.map(MagnitudeValue.fromString.reverseGet).orEmpty).setSortByAuto
          ) ++
          List(
            column("epoch", ScienceTarget.Sidereal.epoch.get)
              .setCell(cell =>
                s"${cell.value.scheme.prefix}${Epoch.fromStringNoScheme.reverseGet(cell.value)}"
              )
              .setSortByAuto,
            column("pmra", ScienceTarget.Sidereal.properMotionRA.getOption)
              .setCell(
                _.value.map(pmRAFormat.reverseGet).orEmpty
              )
              .setSortByAuto,
            column("pmdec", ScienceTarget.Sidereal.properMotionDec.getOption)
              .setCell(_.value.map(pmDecFormat.reverseGet).orEmpty)
              .setSortByAuto,
            column("rv", ScienceTarget.Sidereal.radialVelocity.get)
              .setCell(_.value.map(formatRV.reverseGet).orEmpty)
              .setSortByAuto,
            column("z", (ScienceTarget.Sidereal.radialVelocity.get _).andThen(rvToRedshiftGet))
              .setCell(_.value.map(formatZ.reverseGet).orEmpty)
              .setSortByAuto,
            column("cz", (ScienceTarget.Sidereal.radialVelocity.get _).andThen(rvToARVGet))
              .setCell(_.value.map(formatCZ.reverseGet).orEmpty)
              .setSortByAuto,
            column("parallax", ScienceTarget.Sidereal.parallax.get)
              .setCell(_.value.map(Parallax.milliarcseconds.get).map(_.toString).orEmpty)
              .setSortByAuto,
            column("morphology", _ => ""),
            column("sed", _ => "")
          )
      }
      // rows
      .useMemoBy((props, _) => props.targets)((_, _) =>
        _.collect { case st @ ScienceTarget(_, SiderealTarget(_, _, _)) =>
          st.asInstanceOf[ScienceTarget[SiderealTarget]]
        }
      )
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
                      .map(col => col: IdType[ScienceTarget[SiderealTarget]])
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
            cell = (cell: Cell[_, _]) =>
              TableCell(clazz = columnClasses.get(cell.column.id.toString).orUndefined)(
                ^.whiteSpace.nowrap
              )
          )(tableInstance)
        )
      )
}
