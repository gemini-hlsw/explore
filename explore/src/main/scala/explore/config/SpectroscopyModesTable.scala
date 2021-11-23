// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.data._
import cats.effect._
import cats.syntax.all._
import coulomb.Quantity
import coulomb.refined._
import crystal.react.View
import crystal.react.hooks._
import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.ObsQueries._
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.itc._
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
import react.semanticui.elements.button.Button
import react.semanticui.modules.popup.Popup
import react.virtuoso._
import react.virtuoso.raw.ListRange
import reactST.reactTable._
import reactST.reactTable.mod.DefaultSortTypes
import reactST.reactTable.mod.SortByFn
import reactST.reactTable.mod.UseTableRowProps
import spire.math.Bounded
import spire.math.Interval

import java.text.DecimalFormat

import scalajs.js.|

final case class SpectroscopyModesTable(
  scienceConfiguration:     View[Option[ScienceConfigurationData]],
  spectroscopyRequirements: SpectroscopyRequirementsData,
  matrix:                   SpectroscopyModesMatrix
)(implicit val ctx:         AppContextIO)
    extends ReactFnProps[SpectroscopyModesTable](SpectroscopyModesTable.component)

object SpectroscopyModesTable {
  type Props = SpectroscopyModesTable

  type ColId = NonEmptyString

  implicit val reuseProps: Reusability[Props] =
    Reusability.by(x => (x.scienceConfiguration, x.spectroscopyRequirements))

  implicit val listRangeReuse: Reusability[ListRange] =
    Reusability.by(x => (x.startIndex.toInt, x.endIndex.toInt))

  protected val ModesTableDef = TableDef[SpectroscopyModeRow].withSortBy.withBlockLayout

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
  val CoverageColumnId: ColId    = "coverage"
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
      CoverageColumnId    -> "Coverage",
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

  def formatWavelengthCoverage(r: Interval[Quantity[BigDecimal, Micrometer]]): String = r match {
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

  private def itcCell(c: EitherNec[ItcQueryProblems, ItcResult]): VdomElement = {
    val content: TagMod = c match {
      case Left(nel)                        =>
        if (nel.exists(_ == ItcQueryProblems.UnsupportedMode))
          Popup(content = "Mode not supported", trigger = Icons.Ban.color("red"))
        else {
          val content = nel.collect {
            case ItcQueryProblems.MissingSignalToNoise => "Set S/N"
            case ItcQueryProblems.MissingWavelength    => "Set Wavelength"
            case ItcQueryProblems.GenericError(e)      => e
          }
          Popup(content = content.mkString_(", "), trigger = Icons.TriangleSolid)
        }
      case Right(ItcResult.Result(t, c))    =>
        val secs = t.toMillis / 1000.0 * c
        f"$secs%.0f s"
      case Right(ItcResult.Pending)         =>
        Icons.Spinner.spin(true)
      case Right(ItcResult.SourceTooBright) =>
        Popup(content = "Source too bright", trigger = Icons.SunBright.color("yellow"))
    }
    <.div(ExploreStyles.ITCCell, content)
  }

  implicit val reuseQueryResponse: Reusability[EitherNec[ItcQueryProblems, ItcResult]] =
    Reusability.byEq

  def sortItcFun(
    itc: ItcResultsCache,
    cw:  Option[Wavelength],
    sn:  Option[PosBigDecimal]
  ): SortByFn[SpectroscopyModeRow] =
    (
      rowA: UseTableRowProps[SpectroscopyModeRow],
      rowB: UseTableRowProps[SpectroscopyModeRow],
      _:    String | String,
      desc: Boolean | Unit
    ) =>
      (itc.forRow(cw, sn, rowA.original), itc.forRow(cw, sn, rowB.original)) match {
        case (Right(ItcResult.Result(e1, t1)), Right(ItcResult.Result(e2, t2))) =>
          (e1.toMillis * t1 - e2.toMillis * t2).toDouble
        case (Left(_), Right(ItcResult.Result(e1, t1)))                         =>
          e1.toMillis * t1.toDouble
        case (Right(ItcResult.Result(e1, t1)), Left(_))                         =>
          -e1.toMillis * t1.toDouble
        case _                                                                  =>
          (desc: Any) match {
            case true  => -Double.MaxValue
            case false => Double.MaxValue
            case _     => 0
          }
      }

  def columns(
    cw:  Option[Wavelength],
    fpu: Option[FocalPlane],
    sn:  Option[PosBigDecimal],
    itc: ItcResultsCache
  ) =
    List(
      column(InstrumentColumnId, SpectroscopyModeRow.instrumentAndConfig.get)
        .setCell(c => formatInstrument(c.value))
        .setWidth(120)
        .setMinWidth(50)
        .setMaxWidth(150),
      column(TimeColumnId, itc.forRow(cw, sn, _))
        .setCell(c => itcCell(c.value))
        .setWidth(80)
        .setMinWidth(80)
        .setMaxWidth(80)
        .setSortType(sortItcFun(itc, cw, sn)),
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
      column(CoverageColumnId, SpectroscopyModeRow.coverageInterval(cw))
        .setCell(c => formatWavelengthCoverage(c.value))
        .setWidth(100)
        .setMinWidth(100)
        .setMaxWidth(100)
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
        .setSortType(DefaultSortTypes.number)
    ).filter { case c => (c.id.toString) != FPUColumnId.value || fpu.isEmpty }

  protected def rowToConf(row: SpectroscopyModeRow): Option[ScienceConfigurationData] =
    row.instrument match {
      case GmosNorthSpectroscopyRow(disperser, _, filter)
          if row.focalPlane === FocalPlane.SingleSlit =>
        ScienceConfigurationData.GmosNorthLongSlit(filter, disperser, row.slitWidth.size).some
      case GmosSouthSpectroscopyRow(disperser, _, filter)
          if row.focalPlane === FocalPlane.SingleSlit =>
        ScienceConfigurationData.GmosSouthLongSlit(filter, disperser, row.slitWidth.size).some
      case _ => none
    }

  protected def equalsConf(
    row:  SpectroscopyModeRow,
    conf: ScienceConfigurationData
  ): Boolean =
    rowToConf(row).exists(_ === conf)

  protected def enabledRow(row: SpectroscopyModeRow): Boolean =
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
                coverage = s.wavelengthCoverage
                  .map(_.micrometer.toValue[BigDecimal].toRefined[Positive])
              )
          val (enabled, disabled) = rows.partition(enabledRow)
          (enabled ++ disabled)
        }
      )
      // itc results cache
      .useSerialStateView(
        ItcResultsCache(
          Map.empty[ItcResultsCache.CacheKey, EitherNec[ItcQueryProblems, ItcResult]]
        )
      )
      // cols
      .useMemoBy { (props, _, itc) => // Memo Cols
        (props.spectroscopyRequirements.wavelength,
         props.spectroscopyRequirements.focalPlane,
         props.spectroscopyRequirements.signalToNoise,
         itc
        )
      }((_, _, _) => { case (wavelength, focalPlane, sn, itc) =>
        columns(wavelength, focalPlane, sn, itc.get)
      })
      // selectedIndex
      .useStateBy((props, rows, _, _) => selectedRowIndex(props.scienceConfiguration.get, rows))
      // Recompute state if conf or requirements change.
      .useEffectWithDepsBy((props, _, _, _, _) =>
        (props.scienceConfiguration, props.spectroscopyRequirements)
      )((_, rows, _, _, selectedIndex) => { case (scienceConfiguration, _) =>
        selectedIndex.setState(selectedRowIndex(scienceConfiguration.get, rows))
      })
      // tableInstance
      .useTableBy((_, rows, _, cols, _) => ModesTableDef(cols, rows))
      // virtuosoRef
      // This useMemo may be deceptive: it actually memoizes the ref, which is a wrapper to a mutable value.
      .useMemo(())(_ => ModesTable.createRef)
      // visibleRange
      .useState(none[ListRange])
      // atTop
      .useState(false)

      // singleEffect
      .useSingleEffect
      // Recalculate ITC values if the wv or sn change or if the rows get modified
      .useEffectWithDepsBy((props, rows, _, _, _, _, _, _, _, _) =>
        (props.spectroscopyRequirements.wavelength,
         props.spectroscopyRequirements.signalToNoise,
         rows
        )
      )((props, _, itcResults, _, _, _, _, _, _, singleEffect) => {
        case (wavelength, signalToNoise, rows) =>
          implicit val ctx = props.ctx

          singleEffect.submit((wavelength, signalToNoise).mapN { (w, sn) =>
            ITCRequests.queryItc[IO](w, sn, rows, itcResults.async)
          }.orEmpty)
      })
      .renderWithReuse {
        (
          props,
          rows,
          _,
          _,
          selectedIndex,
          tableInstance,
          virtuosoRef,
          visibleRange,
          atTop,
          _
        ) =>
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
              <.label(s"${rows.length} matching configurations", HelpIcon("configuration/table.md"))
            ),
            <.div(
              ExploreStyles.ExploreTable,
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
                  row = (rowData: ModesTableDef.RowType) =>
                    TableRow(
                      disabled = !enabledRow(rowData.original),
                      clazz = ExploreStyles.TableRowSelected.when_(
                        selectedIndex.value.exists(_ === rowData.index.toInt)
                      )
                    )(
                      ^.onClick --> (
                        props.scienceConfiguration.set(toggleRow(rowData.original)) >>
                          selectedIndex.setState(rowData.index.toInt.some)
                      ),
                      props2Attrs(rowData.getRowProps())
                    )
                )(
                  tableInstance,
                  initialIndex = selectedIndex.value.map(idx => (idx - 2).max(0)),
                  rangeChanged = (
                    (range: ListRange) => visibleRange.setState(range.some)
                  ).some,
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
