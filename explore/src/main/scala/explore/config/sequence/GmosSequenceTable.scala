// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import react.common.Css
import react.common.ReactFnProps
import reactST.{tanstackTableCore => raw}

case class GmosSequenceTable(atoms: List[Atom]) extends ReactFnProps(GmosSequenceTable.component)

object GmosSequenceTable:
  private type Props = GmosSequenceTable

  private val ColDef = ColumnDef[GmosSequenceRow.FutureStep]

  private def drawBracket(rows: Int): VdomElement =
    svg(^.width   := "1px", ^.height := "15px", ^.overflow.visible)(
      use(
        transform := s"scale(1, ${math.pow(rows.toDouble, 1.08)})",
        xlinkHref := "#bracket"
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

  val bracketDef =
    svg(^.width := "0", ^.height := "0")(
      defs(
        path(
          transform   := "scale(1, 0.28)",
          fill        := "white", // FIXME Use CSS
          strokeWidth := "0",
          d           := "M 2.5255237,42.511266 C 2.9018235,41.543703 2.9183988,40.479268 2.9295801,39.441167 3.0257633,30.51126 3.0823959,21.580072 2.947325,12.650669 2.9231886,11.055039 2.8933167,9.4523308 3.1035398,7.8704257 3.3137629,6.2885207 3.7758163,4.7150177 4.6625942,3.3882765 5.8680949,1.5846823 7.8548731,0.32344155 10,0 9.1651831,0.77722338 8.4802709,1.7148791 7.9937845,2.746541 6.9576584,4.9437899 6.8533308,7.4514513 6.8235522,9.8805609 6.7206706,18.272857 7.2905092,26.672179 6.8823909,35.055177 6.8167718,36.403033 6.7250316,37.755886 6.4343209,39.073653 6.1436102,40.39142 5.6454801,41.680731 4.8313656,42.756947 4.0971435,43.727549 3.1128448,44.507326 2,45 c 1.2050792,0.603993 2.2555169,1.513477 3.0257355,2.619726 0.967061,1.388969 1.4785617,3.053394 1.7173188,4.728935 0.2387572,1.675541 0.2181075,3.375775 0.2046929,5.068188 -0.065798,8.301234 0.054193,16.603325 -0.040718,24.904278 -0.019251,1.683679 -0.035532,3.428545 0.6452292,4.968581 C 8.0528414,88.422141 8.9242492,89.387018 10,90 8.1813551,89.702562 6.4820251,88.725349 5.3102118,87.3031 4.2259102,85.987066 3.606374,84.337657 3.2912749,82.661838 2.9761757,80.986019 2.9488582,79.270938 2.9359838,77.565801 2.869984,68.824508 3.1582519,60.082204 3.0067424,51.341975 2.9840763,50.034421 2.9431715,48.687654 2.4144109,47.491567 1.9369295,46.411476 1.0645415,45.51121 0,45 1.1412417,44.575325 2.0841488,43.646153 2.5255237,42.511266",
          id          := "bracket"
        )
      )
    )

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
