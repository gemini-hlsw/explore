// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import algebra.instances.all.given
import boopickle.DefaultBasic.*
import cats.data.*
import cats.effect.*
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all.*
import coulomb.*
import crystal.Pot
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.UserPreferencesQueries.TableStore
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.InstrumentConfigAndItcResult
import explore.model.Progress
import explore.model.ScienceRequirements
import explore.model.ScienceRequirements.*
import explore.model.display.*
import explore.model.enums.ExposureTimeModeType
import explore.model.enums.ExposureTimeModeType.*
import explore.model.enums.TableId
import explore.model.enums.WavelengthUnits
import explore.model.itc.*
import explore.model.reusability.given
import explore.modes.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.OptionLike.optionInstance
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.geom.flamingos2.flamingos2SlitWidthPixels
import lucuma.core.geom.gmos.gmosSlitWidthPixels
import lucuma.core.math.*
import lucuma.core.math.units.Pixels
import lucuma.core.model.*
import lucuma.core.syntax.all.*
import lucuma.core.util.Display
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.react.common.ReactFnProps
import lucuma.react.floatingui.Placement
import lucuma.react.floatingui.syntax.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.schemas.model.CentralWavelength
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import lucuma.ui.table.ColumnSize.*
import lucuma.ui.table.hooks.*

import java.text.DecimalFormat
import scala.language.implicitConversions

case class SpectroscopyModesTable(
  userId:                   Option[User.Id],
  selectedConfig:           View[Option[InstrumentConfigAndItcResult]],
  exposureTimeMode:         Option[ExposureTimeMode],
  spectroscopyRequirements: ScienceRequirements.Spectroscopy,
  constraints:              ConstraintSet,
  targets:                  List[ItcTarget],
  baseCoordinates:          Option[CoordinatesAtVizTime],
  matrix:                   SpectroscopyModesMatrix,
  customSedTimestamps:      List[Timestamp],
  units:                    WavelengthUnits
) extends ReactFnProps(SpectroscopyModesTable.component):
  val validTargets: Option[NonEmptyList[ItcTarget]] =
    NonEmptyList.fromList(targets.filter(_.canQueryITC))

private object SpectroscopyModesTable extends ModesTableCommon:
  private type Props = SpectroscopyModesTable

  private given Reusability[SpectroscopyModeRow]     = Reusability.by(_.id)
  private given Reusability[SpectroscopyModesMatrix] = Reusability.by(_.matrix.length)

  private case class SpectroscopyModeRowWithResult(
    entry:              SpectroscopyModeRow,
    result:             Pot[EitherNec[ItcTargetProblem, ItcResult]],
    wavelengthInterval: Option[BoundedInterval[Wavelength]]
  ) extends TableRowWithResult:
    val rowId: RowId                = RowId(entry.id.orEmpty.toString)
    val config: ItcInstrumentConfig = entry.instrument

  private val ColDef = ColumnDef[SpectroscopyModeRowWithResult].WithTableMeta[TableMeta]

  private val decFormat = new DecimalFormat("0.###")

  private def column[V](
    id:       ColumnId,
    accessor: SpectroscopyModeRowWithResult => V
  ): ColumnDef.Single.WithTableMeta[SpectroscopyModeRowWithResult, V, TableMeta] =
    ColDef(id, accessor, columnNames.getOrElse(id, id.value))

  private val InstrumentColumnId: ColumnId         = ColumnId("instrument")
  private val SlitWidthColumnId: ColumnId          = ColumnId("slit_width")
  private val SlitLengthColumnId: ColumnId         = ColumnId("slit_length")
  private val GratingColumnId: ColumnId            = ColumnId("grating")
  private val FilterColumnId: ColumnId             = ColumnId("filter")
  private val WavelengthIntervalColumnId: ColumnId = ColumnId("interval")
  private val FPUColumnId: ColumnId                = ColumnId("fpu")
  private val ResolutionColumnId: ColumnId         = ColumnId("resolution")
  private val AvailablityColumnId: ColumnId        = ColumnId("availability")
  private val TimeColumnId: ColumnId               = ColumnId("time")
  private val SNColumnId: ColumnId                 = ColumnId("sn")

  private val IntervalPrefix: String = "λ Interval"

  private val columnNames: Map[ColumnId, String] =
    Map[ColumnId, String](
      InstrumentColumnId         -> "Instrument",
      SlitWidthColumnId          -> "Slit Width",
      SlitLengthColumnId         -> "Slit Length",
      GratingColumnId            -> "Grating",
      FilterColumnId             -> "Filter",
      FPUColumnId                -> "FPU",
      WavelengthIntervalColumnId -> IntervalPrefix,
      ResolutionColumnId         -> "λ / Δλ",
      AvailablityColumnId        -> "Avail.",
      TimeColumnId               -> "Time",
      SNColumnId                 -> "S/N"
    )

  private val formatSlitWidth: ModeSlitSize => String = ss =>
    decFormat.format(
      ModeSlitSize.milliarcseconds.get(ss.value).setScale(3, BigDecimal.RoundingMode.UP)
    )

  private val formatSlitLength: ModeSlitSize => String = ss =>
    f"${ModeSlitSize.milliarcseconds.get(ss.value).setScale(0, BigDecimal.RoundingMode.DOWN)}%1.0f"

  private def formatInstrument(r: (Instrument, NonEmptyString)): String = r match
    case (i @ Instrument.Gnirs, m) => s"${i.longName} $m"
    case (i, _)                    => i.longName

  private def formatFPU(r: FocalPlane): String = r match
    case FocalPlane.SingleSlit   => "Single"
    case FocalPlane.MultipleSlit => "Multi"
    case FocalPlane.IFU          => "IFU"

  private def slitWidthCell(config: ItcInstrumentConfig, slitWidth: ModeSlitSize): VdomNode =
    def fmtGmos(px: Quantity[BigDecimal, Pixels], xBin: GmosXBinning) =
      f"$px%2.1f px (xbin=${xBin.count})"

    val width = config match {
      case ItcInstrumentConfig.GmosNorthSpectroscopy(
            _,
            _,
            _,
            Some(InstrumentOverrides.GmosSpectroscopy(_, ccd, _))
          ) =>
        val px = gmosSlitWidthPixels(slitWidth.value, ccd.xBin)
        fmtGmos(px, ccd.xBin)
      case ItcInstrumentConfig.GmosSouthSpectroscopy(
            _,
            _,
            _,
            Some(InstrumentOverrides.GmosSpectroscopy(_, ccd, _))
          ) =>
        val px = gmosSlitWidthPixels(slitWidth.value, ccd.xBin)
        fmtGmos(px, ccd.xBin)
      case ItcInstrumentConfig.Flamingos2Spectroscopy(_, _, _) =>
        val px = flamingos2SlitWidthPixels(slitWidth.value)
        f"$px%2.1f px"
      case _                                                   => ""
    }

    <.span(formatSlitWidth(slitWidth))
      .withTooltip(tooltip = width, placement = Placement.RightStart)

  private def columns(units: WavelengthUnits) =

    given Display[BoundedInterval[Wavelength]] = wavelengthIntervalDisplay(units)

    List(
      column(
        InstrumentColumnId,
        row => formatInstrument(SpectroscopyModeRow.instrumentAndConfig.get(row.entry))
      )
        .withCell(_.value: String)
        .withColumnSize(Resizable(120.toPx, min = 50.toPx, max = 150.toPx))
        .sortable,
      column(TimeColumnId, _.totalItcTime)
        .withHeader(progressingCellHeader("Time"))
        .withCell: cell =>
          itcCell(cell.row.original.result, TimeOrSNColumn.Time)
        .withColumnSize(FixedSize(85.toPx))
        .withSortUndefined(UndefinedPriority.Last)
        .sortable,
      column(SNColumnId, _.totalSN)
        .withHeader(progressingCellHeader("S/N"))
        .withCell: cell =>
          itcCell(cell.row.original.result, TimeOrSNColumn.SN)
        .withColumnSize(FixedSize(85.toPx))
        .withSortUndefined(UndefinedPriority.Last)
        .sortable,
      column(SlitWidthColumnId,
             row =>
               (SpectroscopyModeRow.instrumentConfig.get(row.entry),
                SpectroscopyModeRow.slitWidth.get(row.entry)
               )
      )
        .withCell(cell => slitWidthCell(cell.value._1, cell.value._2.value))
        .withColumnSize(FixedSize(100.toPx))
        .sortableBy(_._2),
      column(SlitLengthColumnId, row => SpectroscopyModeRow.slitLength.get(row.entry))
        .withCell(cell => formatSlitLength(cell.value.value))
        .withColumnSize(FixedSize(105.toPx))
        .sortable,
      column(GratingColumnId, row => SpectroscopyModeRow.instrumentConfig.get(row.entry))
        .withCell(_.value.gratingStr)
        .withColumnSize(FixedSize(96.toPx))
        .sortableBy(_.gratingStr),
      column(FilterColumnId, row => SpectroscopyModeRow.instrumentConfig.get(row.entry))
        .withCell(_.value.filterStr)
        .withColumnSize(FixedSize(69.toPx))
        .sortableBy(_.filterStr),
      column(FPUColumnId, row => SpectroscopyModeRow.fpu.get(row.entry))
        .withCell(cell => formatFPU(cell.value))
        .withColumnSize(FixedSize(62.toPx))
        .sortable,
      column(WavelengthIntervalColumnId, row => row.wavelengthInterval)
        .withHeader(s"$IntervalPrefix ${units.symbol}")
        .withCell(cell => cell.value.fold("-")(_.shortName))
        .withColumnSize(FixedSize(100.toPx))
        .sortableBy(_.map(_.lower)),
      column(ResolutionColumnId, row => SpectroscopyModeRow.resolution.get(row.entry))
        .withCell(_.value.toString)
        .withColumnSize(FixedSize(70.toPx))
        .sortable
      // TODO Enable this column when we have the data
      // column(AvailablityColumnId, _.configurationSummary)
      //   .withCell(_.value.fold("No")(_ => "Yes"))
      //   .withColumnSize(FixedSize(66.toPx))
      //   .sortable
    )

  private val component =
    ScalaFnComponent[Props]: props =>
      for {
        ctx            <- useContext(AppContext.ctx)
        itcResults     <- useStateView(ItcResultsCache.Empty)
        itcProgress    <- useStateView(none[Progress])
        rows           <- useMemo(
                            (props.matrix,
                             props.spectroscopyRequirements,
                             props.baseCoordinates.map(_.value.dec),
                             itcResults.get.cache.size,
                             props.validTargets,
                             props.constraints,
                             props.customSedTimestamps
                            )
                          ): (matrix, s, dec, _, asterism, constraints, customSedTimestamps) =>

                            val rows: List[SpectroscopyModeRow] =
                              matrix
                                .filtered(
                                  focalPlane = s.focalPlane,
                                  capability = s.capability,
                                  wavelength = s.wavelength,
                                  slitLength = s.focalPlaneAngle.map(s => SlitLength(ModeSlitSize(s))),
                                  resolution = s.resolution,
                                  range = s.wavelengthCoverage,
                                  declination = dec
                                )

                            val sortedRows: List[SpectroscopyModeRow]    = rows.sortBy(_.enabled)
                            // Computes the mode overrides for the current parameters
                            val fixedModeRows: List[SpectroscopyModeRow] =
                              sortedRows
                                .map(
                                  _.withModeOverridesFor(
                                    // Should this wv come from the grating, or is the obs wavelength?
                                    s.wavelength,
                                    asterism.map(_.map(_.sourceProfile)),
                                    constraints.imageQuality
                                  )
                                )
                                .flattenOption

                            fixedModeRows.map: row =>
                              val result = (s.wavelength, asterism, props.exposureTimeMode).mapN {
                                (_, _, exposureMode) =>
                                  itcResults.get.forRow(
                                    exposureMode,
                                    constraints,
                                    asterism,
                                    customSedTimestamps,
                                    row
                                  )
                              }
                              SpectroscopyModeRowWithResult(
                                row,
                                Pot.fromOption(result),
                                s.wavelength.flatMap: w =>
                                  row.wavelengthInterval(w)
                              )
        cols           <- useMemo((props.exposureTimeMode.map(_.modeType), props.units)): (m, u) =>
                            m match
                              case Some(ExposureTimeModeType.SignalToNoise) | None =>
                                columns(u).filterNot(_.id.value === SNColumnId.value)
                              case Some(ExposureTimeModeType.TimeAndCount)         =>
                                columns(u).filterNot(_.id.value === TimeColumnId.value)
        table          <- useReactTableWithStateStore:
                            import ctx.given

                            TableOptionsWithStateStore(
                              TableOptions(
                                cols,
                                rows,
                                getRowId = (row, _, _) => row.rowId,
                                enableSorting = true,
                                state = PartialTableState(
                                  columnVisibility = ColumnVisibility( // Hide FPU column if empty
                                    FPUColumnId -> Visibility.fromVisible:
                                      props.spectroscopyRequirements.focalPlane.isDefined
                                  )
                                ),
                                meta = TableMeta(itcProgress.get)
                              ),
                              TableStore(props.userId, TableId.SpectroscopyModes, cols)
                            )
        // We need to have an indicator of whether we need to scrollTo the selectedIndex as
        // a state because otherwise the scrollTo effect below would often run in the same "hook cyle"
        // as the index change, and it would use the old index so it would scroll to the wrong location.
        // By having it as state with the following `useEffectWithDepsBy`, the scrollTo effect will run
        // in the following "hook cycle" and get the proper index.
        scrollTo       <- useStateView(ScrollTo.Scroll)
        _              <- useEffectWithDeps(table.getState().sorting)(_ => scrollTo.set(ScrollTo.Scroll))
        sortedRows     <- useMemo((rows, table.getState().sorting))(_ =>
                            table.getSortedRowModel().rows.map(_.original).toList
                          )
        itcHookData    <- useItc(
                            itcResults,
                            itcProgress,
                            scrollTo.set(ScrollTo.Scroll),
                            props.exposureTimeMode,
                            props.constraints,
                            props.validTargets,
                            props.customSedTimestamps,
                            sortedRows
                          )
        selectedRow    <- useState:
                            props.selectedConfig.get
                              .flatMap: c =>
                                rows.value.find: row =>
                                  c.instrumentConfig === row.entry.instrument
                              .map(_.entry)
        // selectedIndex
        // The selected index needs to be the index into the sorted data, because that is what
        // the virtualizer uses for scrollTo.
        selectedIndex  <- useMemo((sortedRows, selectedRow.value)): (sortedRows, selectedRow) =>
                            selectedRow.map(sRow => sortedRows.indexWhere(row => row.entry === sRow))
        // Set the selected config if the rows change because it might have different itc data.
        // Note, we use rows for the dependency, not sorted rows, because sorted rows also changes with sort.
        _              <- useEffectWithDeps(rows): _ =>
                            val optRow: Option[SpectroscopyModeRowWithResult] =
                              selectedIndex.value.flatMap(idx => sortedRows.lift(idx))
                            val conf: Option[InstrumentConfigAndItcResult]    =
                              optRow.map: row =>
                                InstrumentConfigAndItcResult(row.entry.instrument, row.result.toOption)
                            if (props.selectedConfig.get =!= conf)
                              props.selectedConfig.set(conf)
                            else Callback.empty
        visibleRows    <- useStateView(none[Range.Inclusive])
        atTop          <- useStateView(false)
        virtualizerRef <- useRef(none[HTMLTableVirtualizer])
        // scroll to the currently selected row.
        _              <- useEffectWithDeps((scrollTo.reuseByValue, selectedIndex.value, rows)):
                            (scrollTo, selectedIndex, _) =>
                              Callback.when(scrollTo.get === ScrollTo.Scroll)(
                                selectedIndex.traverse_(scrollToVirtualizedIndex(_, virtualizerRef)) >>
                                  scrollTo.set(ScrollTo.NoScroll)
                              )
      } yield

        def toggleRow(
          row: SpectroscopyModeRowWithResult
        ): Option[InstrumentConfigAndItcResult] =
          Option.when(
            props.selectedConfig.get.forall(_.instrumentConfig =!= row.entry.instrument)
          ):
            InstrumentConfigAndItcResult(row.entry.instrument, row.result.toOption)

        val errLabel = itcHookData.errorLabel(props.spectroscopyRequirements.wavelength.isDefined)

        val selectedTarget: Option[VdomNode] = findSelectedTarget(
          rows.value,
          props.validTargets
        )

        React.Fragment(
          <.div(ExploreStyles.ModesTableTitle)(
            <.label(
              ExploreStyles.ModesTableCount,
              s"${rows.length} available configurations",
              HelpIcon("configuration/table.md".refined)
            ),
            <.div(
              ExploreStyles.ModesTableInfo,
              errLabel.toTagMod,
              selectedTarget
            )
          ),
          <.div(
            ExploreStyles.ExploreTable,
            ExploreStyles.ExploreBorderTable,
            ExploreStyles.ModesTable
          )(
            PrimeAutoHeightVirtualizedTable(
              table,
              estimateSize = _ => 32.toPx,
              striped = true,
              compact = Compact.Very,
              containerMod = ^.overflow.auto,
              rowMod = row =>
                TagMod(
                  ^.disabled := !row.original.entry.enabled,
                  ExploreStyles.TableRowSelected
                    .when:
                      props.selectedConfig.get
                        .exists(_.instrumentConfig === row.original.entry.instrument)
                  ,
                  (
                    ^.onClick --> (
                      props.selectedConfig.set(toggleRow(row.original)) >>
                        selectedRow.setState(row.original.entry.some)
                    )
                  )
                    .when(row.original.entry.enabled)
                ),
              onChange = tableOnChangeHandler(visibleRows, atTop),
              virtualizerRef = virtualizerRef,
              emptyMessage = <.div(ExploreStyles.SpectroscopyTableEmpty, "No matching modes")
            ),
            scrollUpButton(
              selectedIndex,
              virtualizerRef,
              visibleRows.get,
              atTop.get
            ),
            scrollDownButton(
              selectedIndex,
              virtualizerRef,
              visibleRows.get
            )
          )
        )
