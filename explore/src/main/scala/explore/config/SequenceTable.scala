// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.all.svg._
import react.common._
import explore.common.SequenceStepsGQL.SequenceSteps.Data.Observations.Nodes.Config

import scalajs.js.JSConverters._
import react.semanticui.elements.segment.Segment
import explore.components.ui.ExploreStyles
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import explore.Icons
import lucuma.core.enum.StepType
import java.text.DecimalFormat
import react.semanticui.collections.table._

final case class SequenceTable(config: Config)
    extends ReactProps[SequenceTable](SequenceTable.component)

object SequenceTable {
  type Props = SequenceTable

  type Step = Config.GmosSouthConfig.ScienceS.Atoms.Steps
  val Step = Config.GmosSouthConfig.ScienceS.Atoms.Steps

  type AtomId = String

  private case class StepLine(atomId: AtomId, step: Step, firstOf: Option[Int]) {
    private def componentToArcSec[A]: Offset.Component[A] => BigDecimal =
      ((c: Offset.Component[A]) => c.toAngle)
        .andThen(Angle.signedDecimalArcseconds.get)
        .andThen(_.setScale(1, BigDecimal.RoundingMode.HALF_UP))

    lazy val id: String                       = s"$atomId-${step.id}"
    lazy val exposureSecs: Long               = step.instrumentConfig.exposure.getSeconds
    // TODO Not in model yet, we are just simulating
    lazy val guided: Boolean                  = step.stepType === StepType.Science
    lazy val (p, q): (BigDecimal, BigDecimal) = step.stepConfig match {
      case Step.StepConfig.Science(Offset(p, q)) =>
        (p, q).bimap(componentToArcSec, componentToArcSec)
      case _                                     => (0, 0)
    }
  }

  private val offsetFormat = new DecimalFormat("#.0")

  private val tableMaker =
    SUITableMaker[StepLine](
      Table(celled = true, selectable = true, striped = true, compact = TableCompact.Very),
      header = TableHeader(),
      headerCell = TableHeaderCell(clazz = ExploreStyles.StepTableHeader)
    )

  val component =
    ScalaComponent
      .builder[Props]
      .render_P { props =>
        val bracketDef =
          svg(^.width := "0", ^.height := "0")(
            defs(
              path(
                transform := "scale(-0.25, 0.14) translate(-60)",
                fill := "white", // FIXME Use CSS
                stroke := "white",
                d := "m 0,190 5,0 c 6,0 10,0 14,-1 4,-1 8,-4 11,-8 3,-4 4,-8 5,-13 1,-5 1,-14 1,-26 0,-9 0,-15 1,-20 1,-5 3,-10 6,-13 3,-3 7,-5 13,-5 l 0,-16 c -6,0 -10,-2 -13,-5 -3,-3 -5,-8 -6,-13 -1,-5 -1,-11 -1,-20 0,-12 0,-21 -1,-26 -1,-5 -2,-9 -5,-13 -3,-4 -7,-7 -11,-8 -4,-1 -8,-1 -14,-1 l -5,0 0,15 3,0 c 7,0 11,1 13,3 3,3 4,4 4,7 l 0,25 c 0,15 2,25 5,31 3,6 9,10 15,13 -6,3 -12,7 -15,13 -3,6 -5,16 -5,31 l 0,25 c 0,3 -1,4 -4,7 -2,2 -6,3 -13,3 l -3,0 0,15 z",
                id := "bracket"
              )
            )
          )

        def drawBracket(rows: Int) =
          svg(^.width := "20px",
              ^.height := "20px",
              ^.maxHeight := "100%",
              ^.display.block,
              ^.overflow.visible
          )(
            use(
              transform := s"scale(1, $rows)",
              xlinkHref := "#bracket"
            )
          )

        val columns = tableMaker.columnArray(
          tableMaker
            .accessorColumn(
              "atomSteps",
              _.firstOf.map((drawBracket _).andThen(_.rawElement)).orNull
            )
            .setHeader(" "),
          tableMaker
            .accessorColumn("stepType", _.step.stepType.toString)
            .setHeader("Type"),
          tableMaker
            .accessorColumn("exposure", _.exposureSecs.toString)
            .setHeader("Exp (sec)"),
          tableMaker
            .accessorColumn(
              "guide",
              s =>
                if (s.guided) Icons.Crosshairs.copy(clazz = ExploreStyles.StepGuided).raw else null
            )
            .setHeader(""),
          tableMaker
            .accessorColumn("p", s => offsetFormat.format(s.p))
            .setHeader("p"),
          tableMaker
            .accessorColumn("q", s => offsetFormat.format(s.q))
            .setHeader("q"),
          tableMaker
            .accessorColumn(
              "lambda",
              _.step.instrumentConfig.grating
                .map(Wavelength.decimalNanometers.reverseGet.compose(_.wavelength))
                .map(_.toInt.toString)
                .orNull
            )
            .setHeader("λ (nm)"),
          tableMaker
            .accessorColumn("fpu", _.step.instrumentConfig.fpu.map(_.builtin.shortName).orNull)
            .setHeader("FPU"),
          tableMaker
            .accessorColumn("disperser",
                            _.step.instrumentConfig.grating.map(_.disperser.shortName).orNull
            )
            .setHeader("Disperser"),
          tableMaker
            .accessorColumn("filter", _.step.instrumentConfig.filter.map(_.shortName).orNull)
            .setHeader("Filter"),
          tableMaker
            .accessorColumn("xbin", _.step.instrumentConfig.readout.xBin.shortName)
            .setHeader("Xbin"),
          tableMaker
            .accessorColumn("ybin", _.step.instrumentConfig.readout.yBin.shortName)
            .setHeader("Ybin"),
          tableMaker
            .accessorColumn("roi", _.step.instrumentConfig.roi.shortName)
            .setHeader("ROI"),
          tableMaker
            .accessorColumn("sn", _ => "")
            .setHeader("S/N")
        )

        val options = tableMaker
          .options(rowIdFn = _.id.toString, columns = columns)

        Segment()(
          bracketDef,
          props.config match {
            case Config.GmosSouthConfig(_, _, science) =>
              val steps: List[StepLine] = science.atoms
                .map(atom =>
                  atom.steps.headOption
                    .map(head => StepLine(atom.id, head, atom.steps.length.some.filter(_ > 1))) ++
                    atom.steps.tail.map(step => StepLine(atom.id, step, none))
                )
                .flatten

              tableMaker.component((options, steps.toJSArray))
            case _                                     => "North config!"
          }
        )
      }
      .build
}
