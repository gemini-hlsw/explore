// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.syntax.all.*
import crystal.react.reuse.*
import explore.Icons
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.all.svg.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.typed.{tanstackTableCore => raw}
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import react.common.Css
import react.common.ReactFnProps

case class GmosSequenceTable(atoms: List[Atom]) extends ReactFnProps(GmosSequenceTable.component)

object GmosSequenceTable:
  private type Props = GmosSequenceTable

  private val ColDef = ColumnDef[GmosSequenceRow.FutureStep]

  private def drawBracket(rows: Int): VdomElement =
    svg(^.width   := "1px", ^.height := "15px")(
      use(
        transform := s"scale(1, ${math.pow(rows.toDouble, 1.08)})",
        xlinkHref := "/bracket.svg#bracket"
      )
    )

  private val AtomStepsColumnId: ColumnId = ColumnId("atomSteps")

  private val columns: List[ColumnDef[GmosSequenceRow.FutureStep, ?]] =
    ColDef(AtomStepsColumnId,
           _.firstOf,
           header = " ",
           cell = _.value.map(drawBracket),
           size = 30.toPx
    )
      +: SequenceColumns.gmosColumns(ColDef, _.some)

  private def buildLines(atoms: List[Atom]): List[GmosSequenceRow.FutureStep] =
    atoms
      .map(atom =>
        atom.steps.headOption
          .map(head =>
            GmosSequenceRow.FutureStep.fromStep(head, atom.id, atom.steps.length.some.filter(_ > 1))
          ) ++
          atom.steps.tail.map(step => GmosSequenceRow.FutureStep.fromStep(step, atom.id, none))
      )
      .flatten

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useMemo(())(_ => columns)
      .useMemoBy((props, _) => props.atoms)((_, _) => buildLines)
      .useReactTableBy((props, cols, rows) =>
        TableOptions(
          cols,
          rows,
          getRowId = (row, _, _) => RowId(row.id.toString),
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
          cellMod = // Hide border between bracket column and next one
            _.column.id match
              case colId if colId === AtomStepsColumnId.value                =>
                ExploreStyles.SequenceBracketCell
              case colId if colId === SequenceColumns.StepTypeColumnId.value =>
                ExploreStyles.CellHideBorder
              case _                                                         => Css.Empty
        )
      }
