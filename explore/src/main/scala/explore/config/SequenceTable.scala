// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all._
import crystal.react.reuse._
import explore.Icons
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.all.svg._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.GmosNorthGrating
import lucuma.core.enum.GmosNorthFilter
import lucuma.core.enum.GmosNorthFpu
import lucuma.core.enum.GmosSouthGrating
import lucuma.core.enum.GmosSouthFilter
import lucuma.core.enum.GmosSouthFpu
import lucuma.core.enum.StepType
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.Atom
import queries.common.SequenceStepsGQL.SequenceSteps.Data.Observations.Nodes.Config
import queries.common.SequenceStepsGQL.SequenceSteps._
import react.common._
import react.semanticui.collections.table._
import react.semanticui.elements.header.Header
import react.semanticui.elements.segment.Segment
import reactST.reactTable._

import java.text.DecimalFormat

final case class SequenceTable(config: Config)
    extends ReactFnProps[SequenceTable](SequenceTable.component)

object SequenceTable {
  type Props = SequenceTable

  trait SiteResolver[Site <: SeqSite] {
    def disperserName(disperser: Site#Disperser): String
    def fpuName(fpu:             Site#Fpu): String
    def filterName(filter:       Site#Filter): String
  }
  implicit object NorthSiteResolver extends SiteResolver[SeqSite.North] {
    def disperserName(disperser: GmosNorthGrating): String = disperser.shortName
    def fpuName(fpu: GmosNorthFpu): String                 = fpu.shortName
    def filterName(filter: GmosNorthFilter): String        = filter.shortName
  }
  implicit object SouthSiteResolver extends SiteResolver[SeqSite.South] {
    def disperserName(disperser: GmosSouthGrating): String = disperser.shortName
    def fpuName(fpu: GmosSouthFpu): String                 = fpu.shortName
    def filterName(filter: GmosSouthFilter): String        = filter.shortName
  }

  private case class StepLine[Site <: SeqSite](
    atomId:            Atom.Id,
    step:              SeqStep[Site],
    firstOf:           Option[Int]
  )(implicit resolver: SiteResolver[Site]) {
    private def componentToArcSec[A]: Offset.Component[A] => BigDecimal =
      ((c: Offset.Component[A]) => c.toAngle)
        .andThen(Angle.signedDecimalArcseconds.get)
        .andThen(_.setScale(1, BigDecimal.RoundingMode.HALF_UP))

    lazy val id: String                       = s"$atomId-${step.id}"
    lazy val exposureSecs: Long               = step.instrumentConfig.exposure.getSeconds
    // TODO Not in model yet, we are just simulating
    lazy val guided: Boolean                  = step.stepType === StepType.Science
    lazy val (p, q): (BigDecimal, BigDecimal) = step.stepConfig match {
      case science: SeqStepConfig.SeqScienceStep =>
        (science.offset.p, science.offset.q).bimap(componentToArcSec, componentToArcSec)
      case _                                     => (0, 0)
    }
    lazy val wavelength: Option[BigDecimal]   = step.instrumentConfig.grating
      .map(Wavelength.decimalNanometers.reverseGet.compose(_.wavelength))
    lazy val disperserName: Option[String]    =
      step.instrumentConfig.grating.map(grating => resolver.disperserName(grating.disperser))
    lazy val fpuName: Option[String]          =
      step.instrumentConfig.fpu.flatMap(_.builtin).map(resolver.fpuName)
    lazy val filterName: Option[String]       =
      step.instrumentConfig.filter.map(resolver.filterName)
  }

  private val offsetFormat = new DecimalFormat("#.0")

  private val StepTable = TableDef[StepLine[_]]

  private def drawBracket(rows: Int): VdomElement =
    svg(^.width   := "1px", ^.height := "15px", ^.overflow.visible)(
      use(
        transform := s"scale(1, $rows)",
        xlinkHref := "#bracket"
      )
    )

  private def rightAligned(value: Any) =
    <.div(^.textAlign.right)(value.toString)

  private val columns = List(
    StepTable
      .Column("atomSteps", _.firstOf)
      .setHeader(" ")
      .setCell(_.value.map(drawBracket)),
    StepTable
      .Column("stepType", _.step.stepType)
      .setHeader("Type")
      .setCell(_.value.toString),
    StepTable
      .Column("exposure", _.exposureSecs)
      .setHeader(rightAligned("Exp (sec)").rawElement)
      .setCell(cell => rightAligned(cell.value)),
    StepTable
      .Column("guide", _.guided)
      .setHeader("")
      .setCell(cell =>
        if (cell.value) Icons.Crosshairs.copy(clazz = ExploreStyles.StepGuided) else EmptyVdom
      ),
    StepTable
      .Column("p", _.p)
      .setHeader(rightAligned("p").rawElement)
      .setCell(cell => rightAligned(offsetFormat.format(cell.value))),
    StepTable
      .Column("q", _.q)
      .setHeader(rightAligned("q").rawElement)
      .setCell(cell => rightAligned(offsetFormat.format(cell.value))),
    StepTable
      .Column("lambda", _.wavelength)
      .setHeader(rightAligned("λ (nm)").rawElement)
      .setCell(_.value.map((rightAligned _).compose(_.toInt))),
    StepTable
      .Column("fpu", _.fpuName)
      .setHeader(rightAligned("FPU").rawElement)
      .setCell(_.value.map(rightAligned)),
    StepTable
      .Column("disperser", _.disperserName)
      .setHeader("Grating")
      .setCell(_.value.orEmpty),
    StepTable
      .Column("filter", _.filterName)
      .setHeader("Filter")
      .setCell(_.value.orEmpty),
    StepTable
      .Column("xbin", _.step.instrumentConfig.readout.xBin)
      .setHeader(rightAligned("Xbin").rawElement)
      .setCell(cell => rightAligned(cell.value.shortName)),
    StepTable
      .Column("ybin", _.step.instrumentConfig.readout.yBin)
      .setHeader(rightAligned("Ybin").rawElement)
      .setCell(cell => rightAligned(cell.value.shortName)),
    StepTable
      .Column("roi", _.step.instrumentConfig.roi)
      .setHeader("ROI")
      .setCell(_.value.shortName),
    StepTable
      .Column("sn", _ => "")
      .setHeader("S/N")
  )

  // Props are not used directly when computing reusability in render.
  implicit private val propsReuse: Reusability[Props] = Reusability.always

  private def buildLines[Site <: SeqSite: SiteResolver](
    atoms: List[SeqAtom[Site]]
  ): List[StepLine[_]] =
    atoms
      .map(atom =>
        atom.steps.headOption
          .map(head => StepLine(atom.id, head, atom.steps.length.some.filter(_ > 1))) ++
          atom.steps.tail.map(step => StepLine(atom.id, step, none))
      )
      .flatten

  private val StepTableComponent = new SUITable(StepTable)

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useTableBy(props =>
        StepTable(
          columns.reuseAlways,
          ((_: Config) match {
            case Config.GmosSouthConfig(_, _, acquisition, _) => buildLines(acquisition.atoms)
            case _                                            => List.empty
          }).reuseCurrying(props.config)
        )
      )
      .useTableBy((props, _) =>
        StepTable(
          columns.reuseAlways,
          ((_: Config) match {
            case Config.GmosSouthConfig(_, _, _, science) => buildLines(science.atoms)
            case _                                        => List.empty
          }).reuseCurrying(props.config)
        )
      )
      .renderWithReuse { (_, acquisitionTable, scienceTable) =>
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

        val FormattedTable = StepTableComponent(
          Table(celled = true, selectable = true, striped = true, compact = TableCompact.Very),
          header = true,
          headerCell = TableHeaderCell(clazz = ExploreStyles.StepTableHeader)
        )

        <.div(^.height := "100%", ^.overflow.auto)(
          Segment()(
            bracketDef,
            <.div(
              Header("Acquisition"),
              FormattedTable(acquisitionTable),
              Header("Science"),
              FormattedTable(scienceTable)
            )
          )
        )
      }
}
