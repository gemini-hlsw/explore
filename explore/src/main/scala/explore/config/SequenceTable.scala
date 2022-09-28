// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

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
import lucuma.react.table.*
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import react.common.Css
import react.common.ReactFnProps
import reactST.{tanstackTableCore => raw}

import java.text.DecimalFormat

case class SequenceTable(atoms: List[Atom]) extends ReactFnProps(SequenceTable.component)

object SequenceTable:
  private type Props = SequenceTable

  private case class StepLine(atomId: Atom.Id, step: Step, firstOf: Option[Int]):
    private def componentToArcSec[A]: Offset.Component[A] => BigDecimal =
      ((c: Offset.Component[A]) => c.toAngle)
        .andThen(Angle.signedDecimalArcseconds.get)
        .andThen(_.setScale(1, BigDecimal.RoundingMode.HALF_UP))

    lazy val id: String                       = s"$atomId-${step.id}"
    lazy val exposureSecs: Long               =
      step.instrumentConfig match {
        case DynamicConfig.GmosNorth(exposure, _, _, _, _, _, _) => exposure.getSeconds
        case DynamicConfig.GmosSouth(exposure, _, _, _, _, _, _) => exposure.getSeconds
      }
    // TODO Not in model yet, we are just simulating
    lazy val guided: Boolean                  =
      step.stepConfig match {
        case StepConfig.Science(_) => true
        case _                     => false
      }
    lazy val (p, q): (BigDecimal, BigDecimal) =
      step.stepConfig match {
        case StepConfig.Science(Offset(p, q)) => (p, q).bimap(componentToArcSec, componentToArcSec)
        case _                                => (BigDecimal(0), BigDecimal(0))
      }
    lazy val wavelength: Option[BigDecimal]   =
      (step.instrumentConfig match {
        case DynamicConfig.GmosNorth(_, _, _, _, grating, _, _) => grating.map(_.wavelength)
        case DynamicConfig.GmosSouth(_, _, _, _, grating, _, _) => grating.map(_.wavelength)
      }).map(Wavelength.decimalNanometers.reverseGet)
    lazy val gratingName: Option[String]      =
      step.instrumentConfig match {
        case DynamicConfig.GmosNorth(_, _, _, _, grating, _, _) =>
          grating.map(_.grating.shortName)
        case DynamicConfig.GmosSouth(_, _, _, _, grating, _, _) =>
          grating.map(_.grating.shortName)
      }
    lazy val fpuName: Option[String]          =
      step.instrumentConfig match {
        case DynamicConfig.GmosNorth(_, _, _, _, _, _, Some(GmosFpuMask.Builtin(fpu))) =>
          fpu.shortName.some
        case DynamicConfig.GmosSouth(_, _, _, _, _, _, Some(GmosFpuMask.Builtin(fpu))) =>
          fpu.shortName.some
        case _                                                                         =>
          none
      }
    lazy val filterName: Option[String]       =
      step.instrumentConfig match {
        case DynamicConfig.GmosNorth(_, _, _, _, _, filter, _) => filter.map(_.shortName)
        case DynamicConfig.GmosSouth(_, _, _, _, _, filter, _) => filter.map(_.shortName)
      }
    lazy val readoutXBin: Option[String]      =
      step.instrumentConfig match {
        case DynamicConfig.GmosNorth(_, readout, _, _, _, _, _) => readout.xBin.shortName.some
        case DynamicConfig.GmosSouth(_, readout, _, _, _, _, _) => readout.xBin.shortName.some
      }
    lazy val readoutYBin: Option[String]      =
      step.instrumentConfig match {
        case DynamicConfig.GmosNorth(_, readout, _, _, _, _, _) => readout.yBin.shortName.some
        case DynamicConfig.GmosSouth(_, readout, _, _, _, _, _) => readout.yBin.shortName.some
      }
    lazy val roi: Option[String]              =
      step.instrumentConfig match {
        case DynamicConfig.GmosNorth(_, _, _, roi, _, _, _) => roi.shortName.some
        case DynamicConfig.GmosSouth(_, _, _, roi, _, _, _) => roi.shortName.some
      }

  private val offsetFormat = new DecimalFormat("#.0")

  private val ColDef = ColumnDef[StepLine]

  private def drawBracket(rows: Int): VdomElement =
    svg(^.width   := "1px", ^.height := "15px", ^.overflow.visible)(
      use(
        transform := s"scale(1, ${math.pow(rows.toDouble, 1.08)})",
        xlinkHref := "#bracket"
      )
    )

  private def rightAligned(value: Any) =
    <.div(^.textAlign.right)(value.toString)

  private val columns = List(
    ColDef("atomSteps", _.firstOf, header = " ", cell = _.value.map(drawBracket)),
    ColDef("stepType", _.step.stepConfig.stepType, header = "Type", cell = _.value.toString),
    ColDef(
      "exposure",
      _.exposureSecs,
      header = _ => rightAligned("Exp (sec)"),
      cell = cell => rightAligned(cell.value)
    ),
    ColDef(
      "guide",
      _.guided,
      header = "",
      cell = cell =>
        if (cell.value) Icons.Crosshairs.copy(clazz = ExploreStyles.StepGuided) else EmptyVdom
    ),
    ColDef(
      "p",
      _.p,
      header = _ => rightAligned("p"),
      cell = cell => rightAligned(offsetFormat.format(cell.value))
    ),
    ColDef(
      "q",
      _.q,
      header = _ => rightAligned("q"),
      cell = cell => rightAligned(offsetFormat.format(cell.value))
    ),
    ColDef(
      "lambda",
      _.wavelength,
      header = _ => rightAligned("λ (nm)"),
      cell = _.value.map((rightAligned _).compose(_.toInt))
    ),
    ColDef("fpu", _.fpuName, header = _ => rightAligned("FPU"), cell = _.value.map(rightAligned)),
    ColDef("grating", _.gratingName, header = "Grating", cell = _.value.orEmpty),
    ColDef("filter", _.filterName, header = "Filter", cell = _.value.orEmpty),
    ColDef(
      "xbin",
      _.readoutXBin,
      header = _ => rightAligned("Xbin"),
      cell = cell => rightAligned(cell.value.orEmpty)
    ),
    ColDef(
      "ybin",
      _.readoutYBin,
      header = _ => rightAligned("Ybin"),
      cell = cell => rightAligned(cell.value.orEmpty)
    ),
    ColDef("roi", _.roi, header = "ROI", cell = _.value.orEmpty),
    ColDef("sn", _ => "", header = "S/N")
  )

  private def buildLines(
    atoms: List[Atom]
  ): List[StepLine] =
    atoms
      .map(atom =>
        atom.steps.headOption
          .map(head => StepLine(atom.id, head, atom.steps.length.some.filter(_ > 1))) ++
          atom.steps.tail.map(step => StepLine(atom.id, step, none))
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
      .useReactTableBy(props =>
        TableOptions(
          columns.reuseAlways,
          Reuse(props.atoms).self.map(buildLines),
          getRowId = (row, _, _) => row.step.id.toString,
          enableColumnResizing = false
        )
      )
      .render { (_, table) =>
        PrimeVirtualizedTable(
          table,
          estimateRowHeightPx = _ => 28,
          compact = Compact.Very,
          hoverableRows = true,
          celled = true,
          cellMod = // Hide border between bracket column and next one
            case cell if cell.column.id === "stepType" => ExploreStyles.CellHideBorder
            case _                                     => Css.Empty
        )
      }
