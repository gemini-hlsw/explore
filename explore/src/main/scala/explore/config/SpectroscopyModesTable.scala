// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all._
import coulomb.Quantity
import coulomb.refined._
import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.string.NonEmptyString
import explore._
import explore.common.ObsQueries._
import explore.components.ui.ExploreStyles
import explore.modes._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.FocalPlane
import lucuma.core.enum._
import lucuma.core.math.Wavelength
import lucuma.core.math.units.Micrometer
import lucuma.core.util.Display
import lucuma.ui.reusability._
import react.common._
import react.common.implicits._
import react.semanticui.collections.table._
import reactST.reactTable._
import reactST.reactTable.mod.DefaultSortTypes
import reactST.reactTable.mod.Row
import reactST.reactTable.util._
import spire.math.Bounded
import spire.math.Interval

import java.text.DecimalFormat

import scalajs.js.JSConverters._

final case class SpectroscopyModesTable(
  scienceConfiguration:     View[Option[ScienceConfigurationData]],
  matrix:                   SpectroscopyModesMatrix,
  spectroscopyRequirements: SpectroscopyRequirementsData
) //extends ReactProps[SpectroscopyModesTable](SpectroscopyModesTable.component)

object SpectroscopyModesTable {
  type Props = SpectroscopyModesTable

  type ColId = NonEmptyString

  implicit def render(props: SpectroscopyModesTable): VdomElement = component(props).vdomElement

  implicit val reuseProps: Reusability[Props] =
    Reusability.by(x => (x.scienceConfiguration, x.spectroscopyRequirements))

  protected val ModesTableMaker = TableMaker[SpectroscopyModeRow].withSort.withBlockLayout

  import ModesTableMaker.syntax._

  val decFormat = new DecimalFormat("0.###");

  protected val ModesTableComponent = new SUITableVirtuoso(ModesTableMaker)

  val disperserDisplay: Display[ModeDisperser] = Display.byShortName {
    case ModeDisperser.NoDisperser      => "-"
    case ModeDisperser.SomeDisperser(t) => t
  }

  def column[V](id: ColId, accessor: SpectroscopyModeRow => V) =
    ModesTableMaker
      .Column(id, accessor)
      .setHeader(columnNames.getOrElse(id, id.value): String)

  val SelectedColumnId: ColId    = "selected"
  val InstrumentColumnId: ColId  = "instrument"
  val SlitWidthColumnId: ColId   = "slit_width"
  val SlitLengthColumnId: ColId  = "slit_length"
  val DisperserColumnId: ColId   = "disperser"
  val FilterColumnId: ColId      = "filter"
  val RangeColumnId: ColId       = "range"
  val FPUColumnId: ColId         = "fpu"
  val ResolutionColumnId: ColId  = "resolution"
  val AvailablityColumnId: ColId = "availability"
  val TimeColumnId: ColId        = "time"

  private val columnNames: Map[ColId, String] =
    Map[NonEmptyString, String](
      InstrumentColumnId  -> "Instrument",
      SlitWidthColumnId   -> "Slit Width",
      SlitLengthColumnId  -> "Slit Length",
      DisperserColumnId   -> "Disperser",
      FilterColumnId      -> "Filter",
      FPUColumnId         -> "FPU",
      RangeColumnId       -> "Range",
      ResolutionColumnId  -> "λ / Δλ",
      AvailablityColumnId -> "Avail.",
      TimeColumnId        -> "Time"
    )

  val formatSlitWidth: ModeSlitSize => String = ss =>
    decFormat.format(
      ModeSlitSize.milliarcseconds.get(ss.size).setScale(3, BigDecimal.RoundingMode.UP)
    )

  val formatSlitLength: ModeSlitSize => String = ss =>
    f"${ModeSlitSize.milliarcseconds.get(ss.size).setScale(0, BigDecimal.RoundingMode.DOWN)}%1.0f"

  def formatDisperser(disperser: InstrumentRow#Disperser): String = disperser match {
    case f: GmosSouthDisperser => f.shortName
    case f: GmosNorthDisperser => f.shortName
    case f: F2Disperser        => f.shortName
    case f: GpiDisperser       => f.shortName
    case f: GnirsDisperser     => f.shortName
    case r                     => r.toString
  }

  def formatFilter(filter: InstrumentRow#Filter): String = filter match {
    case Some(f: GmosSouthFilter) => f.shortName
    case Some(f: GmosNorthFilter) => f.shortName
    case f: F2Filter              => f.shortName
    case f: GnirsFilter           => f.shortName
    case _: None.type             => "none"
    case r                        => r.toString
  }

  def formatWavelengthRange(r: Interval[Quantity[BigDecimal, Micrometer]]): String = r match {
    case Bounded(a, b, _) =>
      List(a, b)
        .map(q => decFormat.format(q.value.setScale(3, BigDecimal.RoundingMode.DOWN)))
        .mkString(" - ")
    case _                =>
      "-"
  }

  def formatInstrument(r: (Instrument, NonEmptyString)): String = r match {
    case (i @ Instrument.Gnirs, m) => s"${i.longName} $m"
    case (i, _)                    => i.longName
  }

  def formatFPU(r: FocalPlane): String = r match {
    case FocalPlane.SingleSlit   => "Single"
    case FocalPlane.MultipleSlit => "Multi"
    case FocalPlane.IFU          => "IFU"
  }

  def columns(cw:              Option[Wavelength], fpu: Option[FocalPlane]) =
    List(
      column(InstrumentColumnId, SpectroscopyModeRow.instrumentAndConfig.get)
        .setCell(c => formatInstrument(c.value))
        .setWidth(120)
        .setMinWidth(50)
        .setMaxWidth(150),
      column(SlitWidthColumnId, SpectroscopyModeRow.slitWidth.get)
        .setCell(c => formatSlitWidth(c.value))
        .setWidth(96)
        .setMinWidth(96)
        .setMaxWidth(96)
        .setSortType(DefaultSortTypes.number),
      column(SlitLengthColumnId, SpectroscopyModeRow.slitLength.get)
        .setCell(c => formatSlitLength(c.value))
        .setWidth(100)
        .setMinWidth(100)
        .setMaxWidth(100)
        .setSortType(DefaultSortTypes.number),
      column(DisperserColumnId, SpectroscopyModeRow.disperser.get)
        .setCell(c => formatDisperser(c.value))
        .setWidth(95)
        .setMinWidth(95)
        .setMaxWidth(95),
      column(FilterColumnId, SpectroscopyModeRow.filter.get)
        .setCell(c => formatFilter(c.value))
        .setWidth(69)
        .setMinWidth(69)
        .setMaxWidth(69),
      column(FPUColumnId, SpectroscopyModeRow.fpu.get)
        .setCell(c => formatFPU(c.value))
        .setWidth(62)
        .setMinWidth(62)
        .setMaxWidth(62),
      column(RangeColumnId, SpectroscopyModeRow.rangeInterval(cw))
        .setCell(c => formatWavelengthRange(c.value))
        .setWidth(74)
        .setMinWidth(74)
        .setMaxWidth(74)
        .setSortType(DefaultSortTypes.number),
      column(ResolutionColumnId, SpectroscopyModeRow.resolution.get)
        .setCell(c => c.value.toString)
        .setWidth(70)
        .setMinWidth(70)
        .setMaxWidth(70)
        .setSortType(DefaultSortTypes.number),
      column(AvailablityColumnId, rowToConf)
        .setCell(_.value.fold("No")(_ => "Yes"))
        .setWidth(66)
        .setMinWidth(66)
        .setMaxWidth(66)
        .setSortType(DefaultSortTypes.number),
      column(TimeColumnId, _ => "N/A")
        .setCell(_ => "N/A")
        .setWidth(66)
        .setMinWidth(66)
        .setMaxWidth(66)
        .setSortType(DefaultSortTypes.number)
    ).filter { case c => (c.id.toString) != FPUColumnId.value || fpu.isEmpty }

  protected def rowToConf(row: SpectroscopyModeRow): Option[ScienceConfigurationData] =
    row.instrument match {
      case GmosNorthSpectroscopyRow(disperser, filter)
          if row.focalPlane === FocalPlane.SingleSlit =>
        ScienceConfigurationData.GmosNorthLongSlit(filter, disperser, row.slitWidth.size).some
      case GmosSouthSpectroscopyRow(disperser, filter)
          if row.focalPlane === FocalPlane.SingleSlit =>
        ScienceConfigurationData.GmosSouthLongSlit(filter, disperser, row.slitWidth.size).some
      case _ => none
    }

  protected def equalsConf(
    row:  SpectroscopyModeRow,
    conf: ScienceConfigurationData
  ): Boolean =
    rowToConf(row).exists(_ === conf)

  // protected def ensureContains(matrix: List[SpectroscopyModeRow], conf: Option[ScienceConfigurationData]): List[SpectroscopyModeRow] =
  //   conf.fold(matrix)(c =>
  //     if(matrix.exists(row => equalsConf(row, c)))
  //       matrix
  //     else

  protected def enabledRow(row: SpectroscopyModeRow): Boolean =
    List(Instrument.GmosNorth, Instrument.GmosSouth).contains_(row.instrument.instrument) &&
      row.focalPlane === FocalPlane.SingleSlit

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useMemoBy(_.spectroscopyRequirements)(props =>
        s => {
          val rows                =
            props.matrix
              .filtered(
                focalPlane = s.focalPlane,
                capabilities = s.capabilities,
                wavelength = s.wavelength,
                slitWidth = s.focalPlaneAngle,
                resolution = s.resolution,
                range = s.wavelengthRange
                  .map(_.micrometer.toValue[BigDecimal].toRefined[Positive])
              )
          val (enabled, disabled) = rows.partition(enabledRow)
          (enabled ++ disabled).toJSArray
        }
      )
      .useMemoBy($ =>
        ($.props.spectroscopyRequirements.wavelength, $.props.spectroscopyRequirements.focalPlane)
      )(_ => { case (wavelength, focalPlane) =>
        columns(wavelength, focalPlane).toJSArray
      })
      // .renderWithReuse{ (props, rows, cols) => // Throws Invalid hook call. Reported to japgolly.
      .render { (props, rows, cols) =>
        def toggleRow(row: SpectroscopyModeRow): Option[ScienceConfigurationData] =
          rowToConf(row).filterNot(conf => props.scienceConfiguration.get.contains_(conf))

        val tableInstance = ModesTableMaker.use(ModesTableMaker.Options(cols, rows))

        React.Fragment(
          <.label(ExploreStyles.ModesTableTitle, s"${rows.length} matching configurations"),
          <.div(
            ExploreStyles.ModesTable,
            ModesTableComponent(
              table = Table(celled = true,
                            selectable = true,
                            striped = true,
                            compact = TableCompact.Very
              )(),
              header = true,
              headerCell = (c: ModesTableMaker.ColumnType) =>
                TableHeaderCell(clazz = ExploreStyles.Sticky |+| ExploreStyles.ModesHeader)(
                  ^.textTransform.capitalize.when(c.id.toString =!= ResolutionColumnId.value),
                  ^.textTransform.none.when(c.id.toString === ResolutionColumnId.value)
                ),
              row = (rowData: Row[SpectroscopyModeRow]) =>
                TableRow(
                  disabled = !enabledRow(rowData.original),
                  clazz = ExploreStyles.ModeSelected.when_(
                    props.scienceConfiguration.get
                      .exists(conf => equalsConf(rowData.original, conf))
                  )
                )(
                  ^.onClick --> props.scienceConfiguration.set(toggleRow(rowData.original)),
                  props2Attrs(rowData.getRowProps())
                )
            )(tableInstance)
          )
        )
      }
}
