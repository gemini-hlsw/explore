// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Order._
import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import crystal.react.reuse._
import explore.Icons
import explore.common.TargetEnvQueriesGQL
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.ScienceTarget
import explore.model.SiderealScienceTarget
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
import lucuma.ui.optics.TruncatedDec
import lucuma.ui.optics.TruncatedRA
import lucuma.ui.optics.ValidFormatInput
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
import reactST.reactTable.mod.Cell
import reactST.reactTable.mod.IdType
import reactST.reactTable.mod.Row
import reactST.reactTable.util._

import scala.collection.immutable.TreeSeqMap

import scalajs.js.JSConverters._

final case class TargetTable(
  targets:          View[TreeSeqMap[ScienceTarget.Id, ScienceTarget]],
  hiddenColumns:    View[Set[String]],
  selectedTarget:   View[Option[ScienceTarget.Id]],
  renderInTitle:    Tile.RenderInTitle
  // undoStacks: View[Map[Target.Id, UndoStacks[IO, SiderealTarget]]],
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[TargetTable](TargetTable.component)

object TargetTable {
  type Props = TargetTable

  protected val TargetTable = TableDef[SiderealScienceTarget].withSort

  import TargetTable.syntax._

  protected val TargetTableComponent = new SUITable(TargetTable)

  implicit protected val propsReuse: Reusability[Props] = Reusability.derive

  private val columnNames: Map[String, String] = Map(
    "delete"       -> " ",
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

  private def deleteSiderealTarget(
    targetId:     ScienceTarget.Id
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

        def column[V](id: String, accessor: SiderealScienceTarget => V) =
          TargetTable
            .Column(id, accessor)
            .setHeader(columnNames(id))

        List(
          column("delete", ScienceTarget.id.get)
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
            .setDisableSortBy(true),
          column("type", _ => ())
            .setCell(_ => Icons.Star)
            .setWidth(30),
          column("name", ScienceTarget.name.get)
            .setCell(cell => cell.value.toString)
            .setSortByFn(_.toString),
          column(
            "ra",
            SiderealScienceTarget.baseRA.get
          ).setCell(cell =>
            TruncatedRA.rightAscension.get
              .andThen(ValidFormatInput.truncatedRA.reverseGet)(cell.value)
          ).setSortByAuto,
          column[Declination](
            "dec",
            SiderealScienceTarget.baseDec.get
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
            column("epoch", SiderealScienceTarget.epoch.get)
              .setCell(cell =>
                s"${cell.value.scheme.prefix}${Epoch.fromStringNoScheme.reverseGet(cell.value)}"
              )
              .setSortByAuto,
            column("pmra", SiderealScienceTarget.properMotionRA.getOption)
              .setCell(
                _.value.map(pmRAFormat.reverseGet).orEmpty
              )
              .setSortByAuto,
            column("pmdec", SiderealScienceTarget.properMotionDec.getOption)
              .setCell(_.value.map(pmDecFormat.reverseGet).orEmpty)
              .setSortByAuto,
            column("rv", SiderealScienceTarget.radialVelocity.get)
              .setCell(_.value.map(formatRV.reverseGet).orEmpty)
              .setSortByAuto,
            column("z", (SiderealScienceTarget.radialVelocity.get _).andThen(rvToRedshiftGet))
              .setCell(_.value.map(formatZ.reverseGet).orEmpty)
              .setSortByAuto,
            column("cz", (SiderealScienceTarget.radialVelocity.get _).andThen(rvToARVGet))
              .setCell(_.value.map(formatCZ.reverseGet).orEmpty)
              .setSortByAuto,
            column("parallax", SiderealScienceTarget.parallax.get)
              .setCell(_.value.map(Parallax.milliarcseconds.get).map(_.toString).orEmpty)
              .setSortByAuto,
            column("morphology", _ => ""),
            column("sed", _ => "")
          )
      }
      // rows
      .useMemoBy((props, _) => props.targets)((_, _) =>
        _.get.collect { case (_, st @ SiderealScienceTarget(_, _)) => st }.toList
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
                      .map(col => col: IdType[SiderealScienceTarget])
                      .toJSArray
                  )
              )
          }.reuseCurrying(props.hiddenColumns.get)
        )
      )
      .render((props, _, _, tableInstance) =>
        <.div(ExploreStyles.ExploreTable)(
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
                          compact = TableCompact.Very
            )(),
            header = true,
            headerCell = (col: TargetTable.ColumnType) =>
              TableHeaderCell(clazz = columnClasses.get(col.id.toString).orUndefined)(
                ^.textTransform.none,
                ^.whiteSpace.nowrap
              ),
            row = (rowData: Row[SiderealScienceTarget]) =>
              TableRow(
                clazz = ExploreStyles.TableRowSelected.when_(
                  props.selectedTarget.get.exists(_ === rowData.original.id)
                )
              )(
                ^.onClick --> props.selectedTarget.set(rowData.original.id.some).toCB,
                props2Attrs(rowData.getRowProps())
              ),
            cell = (cell: Cell[_, _]) =>
              TableCell(clazz = columnClasses.get(cell.column.id.toString).orUndefined)(
                ^.whiteSpace.nowrap
              )
          )(tableInstance)
        )
      )
}
