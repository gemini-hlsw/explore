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
import react.virtuoso._
import react.virtuoso.raw.ListRange

import java.text.DecimalFormat
import react.semanticui.elements.button.Button

final case class SpectroscopyModesTable(
  scienceConfiguration:     View[Option[ScienceConfigurationData]],
  matrix:                   SpectroscopyModesMatrix,
  spectroscopyRequirements: SpectroscopyRequirementsData
) extends ReactFnProps[SpectroscopyModesTable](SpectroscopyModesTable.component)

object SpectroscopyModesTable {
  type Props = SpectroscopyModesTable

  type ColId = NonEmptyString

  implicit val reuseProps: Reusability[Props]         =
    Reusability.by(x => (x.scienceConfiguration, x.spectroscopyRequirements))

  implicit val listRangeReuse: Reusability[ListRange] =
    Reusability.by(x => (x.startIndex.toInt, x.endIndex.toInt))

  protected val ModesTableDef                         = TableDef[SpectroscopyModeRow].withSort.withBlockLayout

  import ModesTableDef.syntax._

  val decFormat = new DecimalFormat("0.###")

  protected val ModesTable = new SUITableVirtuoso(ModesTableDef)

  val disperserDisplay: Display[ModeDisperser] = Display.byShortName {
    case ModeDisperser.NoDisperser      => "-"
    case ModeDisperser.SomeDisperser(t) => t
  }

  def column[V](id: ColId, accessor: SpectroscopyModeRow => V) =
    ModesTableDef
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

  def columns(cw: Option[Wavelength], fpu: Option[FocalPlane])                        =
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

  protected def enabledRow(row: SpectroscopyModeRow): Boolean                         =
    List(Instrument.GmosNorth, Instrument.GmosSouth).contains_(row.instrument.instrument) &&
      row.focalPlane === FocalPlane.SingleSlit

  protected def selectedRowIndex(
    scienceConfiguration: Option[ScienceConfigurationData],
    rows:                 List[SpectroscopyModeRow]
  ): Option[Int] =
    scienceConfiguration
      .map(selected => rows.indexWhere(row => equalsConf(row, selected)))
      .filterNot(_ == -1)

  val component =
    ScalaFnComponent
      .withHooks[Props]
      // rows
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
          (enabled ++ disabled)
        }
      )
      // cols
      .useMemoBy((props, _) => // Memo Cols
        (props.spectroscopyRequirements.wavelength, props.spectroscopyRequirements.focalPlane)
      )((_, _) => { case (wavelength, focalPlane) =>
        columns(wavelength, focalPlane)
      })
      // selectedIndex
      .useStateBy((props, rows, _) => selectedRowIndex(props.scienceConfiguration.get, rows))
      .useEffectWithDepsBy((props, _, _, _) => // Recompute state if conf or requirements change.
        (props.scienceConfiguration, props.spectroscopyRequirements)
      )((_, rows, _, selectedIndex) => { case (scienceConfiguration, _) =>
        selectedIndex.setState(selectedRowIndex(scienceConfiguration.get, rows))
      })
      // tableInstance
      .useTableBy((_, rows, cols, _) => ModesTableDef(cols, rows))
      // virtuosoRef
      // If useRef can be used here, I'm not figuring out how to do that.
      // This useMemo may be deceptive: it actually memoizes the ref, which is a wrapper to a mutable value.
      .useMemo(())(_ => ModesTable.createRef)
      // visibleRange
      .useState(none[ListRange])
      // atTop
      .useState(false)
      .renderWithReuse {
        (props, rows, _, selectedIndex, tableInstance, virtuosoRef, visibleRange, atTop) =>
          def toggleRow(row: SpectroscopyModeRow): Option[ScienceConfigurationData] =
            rowToConf(row).filterNot(conf => props.scienceConfiguration.get.contains_(conf))

          def scrollButton(
            content:        VdomNode,
            style:          Css,
            indexDiff:      Int => Int,
            indexCondition: Int => Boolean
          ): TagMod =
            selectedIndex.value.whenDefined(idx =>
              Button(
                compact = true,
                onClick = virtuosoRef.foreach(
                  _.raw.scrollIntoView(
                    ScrollIntoViewLocation(index = indexDiff(idx - 2),
                                           behavior = ScrollBehavior.Smooth
                    )
                  )
                )
              )(
                ExploreStyles.ScrollButton,
                style
              )(content).when(indexCondition(idx))
            )

          React.Fragment(
            <.div(ExploreStyles.ModesTableTitle)(
              <.label(s"${rows.length} matching configurations")
            ),
            <.div(
              ExploreStyles.ModesTable,
              ModesTable
                .Component(
                  table = Table(celled = true,
                                selectable = true,
                                striped = true,
                                compact = TableCompact.Very
                  )(),
                  header = true,
                  headerCell = (c: ModesTableDef.ColumnType) =>
                    TableHeaderCell(clazz = ExploreStyles.Sticky |+| ExploreStyles.ModesHeader)(
                      ^.textTransform.capitalize.when(c.id.toString =!= ResolutionColumnId.value),
                      ^.textTransform.none.when(c.id.toString === ResolutionColumnId.value)
                    ),
                  row = (rowData: Row[SpectroscopyModeRow]) =>
                    TableRow(
                      disabled = !enabledRow(rowData.original),
                      clazz = ExploreStyles.ModeSelected.when_(
                        selectedIndex.value.exists(_ === rowData.index.toInt)
                      )
                    )(
                      ^.onClick --> (
                        props.scienceConfiguration.set(toggleRow(rowData.original)).toCB >>
                          selectedIndex.setState(rowData.index.toInt.some)
                      ),
                      props2Attrs(rowData.getRowProps())
                    )
                )(
                  tableInstance,
                  initialIndex = selectedIndex.value.map(idx => (idx - 2).max(0)),
                  rangeChanged = ((range: ListRange) => visibleRange.setState(range.some)).some,
                  atTopChange = ((value: Boolean) => atTop.setState(value)).some
                )
                .withRef(virtuosoRef),
              scrollButton(
                Icons.ChevronDoubleUp,
                ExploreStyles.SelectedUp,
                _ - 1,
                idx =>
                  visibleRange.value.exists(range =>
                    (range.startIndex.toInt > 0 || !atTop.value) && range.startIndex > idx - 2
                  )
              ),
              scrollButton(
                Icons.ChevronDoubleDown,
                ExploreStyles.SelectedDown,
                _ + 1,
                idx => visibleRange.value.exists(_.endIndex < idx - 1)
              )
            )
          )
      }
}
