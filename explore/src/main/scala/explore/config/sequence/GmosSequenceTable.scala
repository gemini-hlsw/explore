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
import lucuma.core.math.SignalToNoise
import lucuma.core.model.sequence.*
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.ui.sequence.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*

sealed trait GmosSequenceTable[D]:
  def atoms: List[Atom[D]]
  def sn: Option[SignalToNoise]

case class GmosNorthSequenceTable(
  atoms: List[Atom[DynamicConfig.GmosNorth]],
  sn:    Option[SignalToNoise]
) extends ReactFnProps(GmosNorthSequenceTable.component)
    with GmosSequenceTable[DynamicConfig.GmosNorth]

case class GmosSouthSequenceTable(
  atoms: List[Atom[DynamicConfig.GmosSouth]],
  sn:    Option[SignalToNoise]
) extends ReactFnProps(GmosSouthSequenceTable.component)
    with GmosSequenceTable[DynamicConfig.GmosSouth]

private sealed trait GmosSequenceTableBuilder[D: Eq]:
  private type Props = GmosSequenceTable[D]

  private case class SequenceTableRow(step: SequenceRow.FutureStep[D], index: StepIndex)

  private val ColDef = ColumnDef[SequenceTableRow]

  private def drawBracket(rows: Int): VdomElement =
    svg(^.width := "1px", ^.height := "15px")(
      use(
        transform := s"scale(1, ${math.pow(rows.toDouble, 1.08)})",
        xlinkHref := "/bracket.svg#bracket"
      )
    )

  private val AtomStepsColumnId: ColumnId = ColumnId("atomSteps")

  private val columns: List[ColumnDef[SequenceTableRow, ?]] =
    ColDef(
      AtomStepsColumnId,
      _.step.firstOf,
      header = " ",
      cell = _.value.map(drawBracket),
      size = 30.toPx
    ) +: SequenceColumns.gmosColumns(
      ColDef,
      _.step.some,
      _.index.some
    )

  protected[sequence] val component =
    ScalaFnComponent
      .withHooks[Props]
      .useMemo(())(_ => columns) // cols
      .useMemoBy((props, _) => props.atoms): (props, _) => // rows
        atoms =>
          SequenceRow.FutureStep
            .fromAtoms(atoms, props.sn.showForFutureStep)
            .zipWithStepIndex
            .map(SequenceTableRow.apply)
      .useReactTableBy: (props, cols, rows) =>
        TableOptions(
          cols,
          rows,
          getRowId = (row, _, _) => RowId(row.step.id.toString),
          enableColumnResizing = false,
          enableSorting = false
        )
      .render: (_, _, _, table) =>
        PrimeVirtualizedTable(
          table,
          estimateSize = _ => 28.toPx,
          compact = Compact.Very,
          hoverableRows = true,
          celled = true,
          tableMod = ExploreStyles.SequenceTable,
          cellMod = // Hide border between bracket column and next one
            _.column.id match
              case colId if colId === AtomStepsColumnId.value                    =>
                ExploreStyles.SequenceBracketCell
              case colId if colId === SequenceColumns.IndexAndTypeColumnId.value =>
                ExploreStyles.CellHideBorder
              case _                                                             =>
                Css.Empty
        )

object GmosNorthSequenceTable extends GmosSequenceTableBuilder[DynamicConfig.GmosNorth]

object GmosSouthSequenceTable extends GmosSequenceTableBuilder[DynamicConfig.GmosSouth]
