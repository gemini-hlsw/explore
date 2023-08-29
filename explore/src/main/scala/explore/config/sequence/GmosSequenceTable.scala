// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.Eq
import cats.syntax.all.*
import explore.components.ui.ExploreStyles
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.all.svg.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.sequence.*
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.ui.sequence.SequenceColumns
import lucuma.ui.sequence.SequenceRow
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*

sealed trait GmosSequenceTable[D]:
  def atoms: List[Atom[D]]

case class GmosNorthSequenceTable(atoms: List[Atom[DynamicConfig.GmosNorth]])
    extends ReactFnProps(GmosNorthSequenceTable.component)
    with GmosSequenceTable[DynamicConfig.GmosNorth]

case class GmosSouthSequenceTable(atoms: List[Atom[DynamicConfig.GmosSouth]])
    extends ReactFnProps(GmosSouthSequenceTable.component)
    with GmosSequenceTable[DynamicConfig.GmosSouth]

private sealed trait GmosSequenceTableBuilder[D: Eq]:
  private type Props = GmosSequenceTable[D]

  private val ColDef = ColumnDef[(SequenceRow.FutureStep[D], Int)]

  private def drawBracket(rows: Int): VdomElement =
    svg(^.width := "1px", ^.height := "15px")(
      use(
        transform := s"scale(1, ${math.pow(rows.toDouble, 1.08)})",
        xlinkHref := "/bracket.svg#bracket"
      )
    )

  private val AtomStepsColumnId: ColumnId = ColumnId("atomSteps")

  private val columns: List[ColumnDef[(SequenceRow.FutureStep[D], Int), ?]] =
    ColDef(
      AtomStepsColumnId,
      _._1.firstOf,
      header = " ",
      cell = _.value.map(drawBracket),
      size = 30.toPx
    ) +: SequenceColumns.gmosColumns(ColDef, _._1.some, _._2.some.map(_ + 1))

  protected[sequence] val component =
    ScalaFnComponent
      .withHooks[Props]
      .useMemo(())(_ => columns)
      .useMemoBy((props, _) => props.atoms)((_, _) =>
        atoms => SequenceRow.FutureStep.fromAtoms(atoms).zipWithIndex
      )
      .useReactTableBy((props, cols, rows) =>
        TableOptions(
          cols,
          rows,
          getRowId = (row, _, _) => RowId(row._1.id.toString),
          enableColumnResizing = false,
          enableSorting = false
        )
      )
      .render { (_, _, _, table) =>
        PrimeVirtualizedTable(
          table,
          estimateSize = _ => 28.toPx,
          compact = Compact.Very,
          hoverableRows = true,
          celled = true,
          tableMod = ExploreStyles.SequenceTable,
          cellMod = // Hide border between bracket column and next one
            _.column.id match
              case colId if colId === AtomStepsColumnId.value                =>
                ExploreStyles.SequenceBracketCell
              case colId if colId === SequenceColumns.StepTypeColumnId.value =>
                ExploreStyles.CellHideBorder
              case _                                                         => Css.Empty
        )
      }

object GmosNorthSequenceTable extends GmosSequenceTableBuilder[DynamicConfig.GmosNorth]

object GmosSouthSequenceTable extends GmosSequenceTableBuilder[DynamicConfig.GmosSouth]
