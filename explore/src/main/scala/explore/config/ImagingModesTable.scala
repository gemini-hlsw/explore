// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.Order
import cats.data.EitherNec
import cats.data.NonEmptyList
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all.*
import crystal.Pot
import crystal.react.*
import crystal.react.hooks.*
import explore.common.UserPreferencesQueries.TableStore
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.InstrumentConfigAndItcResult
import explore.model.Progress
import explore.model.ScienceRequirements
import explore.model.display.*
import explore.model.display.given
import explore.model.enums.ExposureTimeModeType
import explore.model.enums.ExposureTimeModeType.*
import explore.model.enums.TableId
import explore.model.enums.WavelengthUnits
import explore.model.itc.*
import explore.model.reusability.given
import explore.modes.*
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.BoundedInterval
import lucuma.core.math.BoundedInterval.*
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.model.ConstraintSet
import lucuma.core.model.CoordinatesAtVizTime
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.User
import lucuma.core.syntax.all.*
import lucuma.core.util.Display
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.react.common.ReactFnProps
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.ui.reusability.given
import lucuma.ui.table.*
import lucuma.ui.table.ColumnSize.*
import lucuma.ui.table.hooks.*

final case class ImagingModesTable(
  userId:              Option[User.Id],
  selectedConfigs:     View[ConfigSelection],
  exposureTimeMode:    Option[ExposureTimeMode],
  imaging:             ScienceRequirements.Imaging,
  matrix:              ImagingModesMatrix,
  constraints:         ConstraintSet,
  targets:             EitherNec[ItcTargetProblem, NonEmptyList[ItcTarget]],
  baseCoordinates:     Option[CoordinatesAtVizTime],
  customSedTimestamps: List[Timestamp],
  units:               WavelengthUnits
) extends ReactFnProps(ImagingModesTable.component)

object ImagingModesTable extends ModesTableCommon:

  private given Reusability[ImagingModesMatrix] = Reusability.by(_.matrix.length)

  extension (interval: BoundedInterval[Wavelength])
    // The 'z' filter, at least, has an upper bound of Int.MaxValue picometers. However,
    // the practical range is limited by things like the detector. According to Andy,
    // the upper limit is 1100 nm.
    private def clipForGmos: BoundedInterval[Wavelength] =
      interval
        .intersect(
          BoundedInterval.unsafeClosed(Wavelength.Min, Wavelength.fromIntNanometers(1100).get)
        )
        .getOrElse(interval)
    private def toWavelengthDelta: Option[WavelengthDelta] =
      // will be 'none' for Point BoundedIntervals (upper === lower)
      WavelengthDelta.fromIntPicometers(
        interval.upper.pm.value.value - interval.lower.pm.value.value
      )

  extension (filter: ItcInstrumentConfig#Filter)
    private def wavelength: Option[Wavelength] =
      filter match
        case g: GmosNorthFilter => g.wavelength.some
        case g: GmosSouthFilter => g.wavelength.some
        case _                  => None

    private def filterTypeStr: String =
      filter match
        case g: GmosNorthFilter => g.filterType.shortName
        case g: GmosSouthFilter => g.filterType.shortName
        case _                  => "-"

    private def wavelengthRangeAndDelta
      : (Option[BoundedInterval[Wavelength]], Option[WavelengthDelta]) =
      def forGmos(
        interval: BoundedInterval[Wavelength]
      ): (Option[BoundedInterval[Wavelength]], Option[WavelengthDelta]) =
        val clipped = interval.clipForGmos
        (clipped.some, clipped.toWavelengthDelta)

      filter match
        case g: GmosNorthFilter => forGmos(g.width)
        case g: GmosSouthFilter => forGmos(g.width)
        case _                  => (None, None)

  private case class ImagingModeRowWithResult(
    entry:  ImagingModeRow,
    result: Pot[EitherNec[ItcTargetProblem, ItcResult]]
  ) extends TableRowWithResult:
    val rowId: RowId                = RowId(entry.id.orEmpty.toString)
    val config: ItcInstrumentConfig = entry.instrument

  private val ColDef = ColumnDef[ImagingModeRowWithResult].WithTableMeta[TableMeta]

  private val InstrumentColumnId: ColumnId  = ColumnId("instrument")
  private val TimeColumnId: ColumnId        = ColumnId("time")
  private val SNColumnId: ColumnId          = ColumnId("sn")
  private val FilterColumnId: ColumnId      = ColumnId("filter")
  private val FilterTypeColumnId: ColumnId  = ColumnId("filter_type")
  private val LambdaColumnId: ColumnId      = ColumnId("lambda")
  private val DeltaLambdaColumnId: ColumnId = ColumnId("delta_lambda")
  private val FovColumnId: ColumnId         = ColumnId("fov")

  private val columnNames: Map[ColumnId, String] =
    Map(
      InstrumentColumnId  -> "Instrument",
      TimeColumnId        -> "Time",
      SNColumnId          -> "S/N",
      FilterColumnId      -> "Filter",
      FilterTypeColumnId  -> "Filter Type",
      LambdaColumnId      -> "λ",
      DeltaLambdaColumnId -> "Δλ",
      FovColumnId         -> "FoV"
    )

  private def column[V](
    id:       ColumnId,
    accessor: ImagingModeRowWithResult => V
  ): ColumnDef.Single.WithTableMeta[ImagingModeRowWithResult, V, TableMeta] =
    ColDef(id, accessor, columnNames.getOrElse(id, id.value))

  private def columns(units: WavelengthUnits) =
    given Display[BoundedInterval[Wavelength]] = wavelengthIntervalDisplay(units)
    given Display[WavelengthDelta]             = wavelengthDeltaDisplay(units)
    given Display[Wavelength]                  = wavelengthDisplay(units)

    given Order[Angle] = Angle.AngleOrder

    List(
      column(InstrumentColumnId, row => ImagingModeRow.instrument.get(row.entry).longName)
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
      column(FilterColumnId, row => ImagingModeRow.instrumentConfig.get(row.entry))
        .withCell(_.value.filterStr)
        .withColumnSize(FixedSize(69.toPx))
        .sortableBy(_.filterStr),
      column(FilterTypeColumnId, row => ImagingModeRow.filter.get(row.entry))
        .withCell(_.value.filterTypeStr)
        .withColumnSize(FixedSize(85.toPx))
        .sortableBy(_.filterTypeStr),
      column(LambdaColumnId, row => ImagingModeRow.filter.get(row.entry).wavelength)
        .withHeader(s"λ ${units.symbol}")
        .withCell(_.value.fold("-")(_.shortName))
        .withColumnSize(FixedSize(75.toPx))
        .sortable,
      column(
        DeltaLambdaColumnId,
        row => ImagingModeRow.filter.get(row.entry).wavelengthRangeAndDelta
      )
        .withHeader(s"Δλ ${units.symbol}")
        .withCell: cell =>
          val (range, delta) = cell.value
          <.span(delta.fold("-")(_.shortName))
            .withOptionalTooltip(range.map(r => s"${r.shortName} ${units.symbol}"))
        .withColumnSize(FixedSize(100.toPx))
        .sortableBy(_._2),
      column(FovColumnId, _.entry.fov)
        .withCell: cell =>
          val arcSeconds = Angle.arcseconds.get(cell.value)
          f"$arcSeconds%.0f\""
        .withColumnSize(FixedSize(75.toPx))
        .sortable
    )

  private val component = ScalaFnComponent[ImagingModesTable]: props =>
    for {
      ctx             <- useContext(AppContext.ctx)
      itcResults      <- useStateView(ItcResultsCache.Empty)
      itcProgress     <- useStateView(none[Progress])
      rows            <- useMemo(
                           props.matrix,
                           props.exposureTimeMode,
                           props.targets,
                           props.constraints,
                           props.customSedTimestamps,
                           props.imaging.minimumFov,
                           props.imaging.allowedFilterTypes,
                           itcResults.get.cache.size
                         ): (matrix, etm, targets, constraints, customSedTimestamps, minimumFov, fts, _) =>
                           matrix
                             .filtered(minimumFov, fts)
                             .map: row =>
                               val result: Option[EitherNec[ItcTargetProblem, ItcResult]] =
                                 etm.map: exposureMode =>
                                   targets.flatMap: asterism =>
                                     itcResults.get.forRow(
                                       exposureMode,
                                       constraints,
                                       asterism.some,
                                       customSedTimestamps,
                                       row
                                     )
                               ImagingModeRowWithResult(
                                 row,
                                 Pot.fromOption(result)
                               )
      cols            <- useMemo((props.exposureTimeMode.map(_.modeType), props.units)): (m, u) =>
                           m match
                             case Some(ExposureTimeModeType.SignalToNoise) | None =>
                               columns(u).filterNot(_.id.value === SNColumnId.value)
                             case Some(ExposureTimeModeType.TimeAndCount)         =>
                               columns(u).filterNot(_.id.value === TimeColumnId.value)
      table           <- useReactTableWithStateStore:
                           import ctx.given

                           TableOptionsWithStateStore(
                             TableOptions(
                               cols,
                               rows,
                               getRowId = (row, _, _) => row.rowId,
                               enableSorting = true,
                               meta = TableMeta(itcProgress.get)
                             ),
                             TableStore(props.userId, TableId.ImagingModes, cols)
                           )
      // We need to have an indicator of whether we need to scrollTo the selectedIndex as
      // a state because otherwise the scrollTo effect below would often run in the same "hook cyle"
      // as the index change, and it would use the old index so it would scroll to the wrong location.
      // By having it as state with the following `useEffectWithDepsBy`, the scrollTo effect will run
      // in the following "hook cycle" and get the proper index.
      scrollTo        <- useStateView(ScrollTo.Scroll)
      _               <- useEffectWithDeps(table.getState().sorting)(_ => scrollTo.set(ScrollTo.Scroll))
      sortedRows      <- useMemo((rows, table.getState().sorting))(_ =>
                           table.getSortedRowModel().rows.map(_.original).toList
                         )
      itcHookData     <- useItc(
                           itcResults,
                           itcProgress,
                           scrollTo.set(ScrollTo.Scroll),
                           props.exposureTimeMode,
                           props.constraints,
                           props.targets.toOption,
                           props.customSedTimestamps,
                           sortedRows
                         )
      // Set the selected config if the rows change because the new rows may no longer contain
      // one or more of the selected rows or the itc results may have changed.
      // Note, we use rows for the dependency, not sorted rows, because sorted rows also changes with sort.
      _               <- useEffectWithDeps(rows): rs =>
                           val oldCfgs = props.selectedConfigs.get.configs
                           val newCfgs =
                             oldCfgs
                               .map(cfg => rs.find(_.entry.instrument === cfg.instrumentConfig))
                               .flattenOption
                               .map(_.configAndResult)
                           if (oldCfgs =!= newCfgs)
                             props.selectedConfigs.set(ConfigSelection.fromList(newCfgs))
                           else Callback.empty
      selectedIndices <- useMemo((sortedRows, props.selectedConfigs.get)):
                           (sortRows, selectedConfigs) =>
                             selectedConfigs.configs
                               .map: c =>
                                 sortRows.indexWhere(_.entry.instrument === c.instrumentConfig)
                               .sorted
      visibleRows     <- useStateView(none[Range.Inclusive])
      atTop           <- useStateView(false)
      virtualizerRef  <- useRef(none[HTMLTableVirtualizer])
      // scroll to the top selected row.
      _               <- useEffectWithDeps((scrollTo.reuseByValue, selectedIndices.value.headOption, rows)):
                           (scrollTo, optFirstIdx, _) =>
                             Callback.when(scrollTo.get === ScrollTo.Scroll)(
                               optFirstIdx.traverse_(scrollToVirtualizedIndex(_, virtualizerRef)) >>
                                 scrollTo.set(ScrollTo.NoScroll)
                             )
    } yield
      val errlabel       = itcHookData.errorLabel(true)
      val selectedTarget = findSelectedTarget(rows.value, props.targets.toOption)
      val selectedCount  = props.selectedConfigs.get.count

      val upIndex   = visibleRows.get.flatMap(vr => selectedIndices.value.findLast(_ < vr.start))
      val downIndex = visibleRows.get.flatMap(vr => selectedIndices.value.find(_ > vr.end))

      def clickHandler(row: ImagingModeRowWithResult): ReactMouseEvent => Callback =
        (e: ReactMouseEvent) =>
          if (e.metaKey || e.ctrlKey)
            props.selectedConfigs.mod(_.toggle(row.configAndResult))
          else
            props.selectedConfigs.mod(_.toggleOrSet(row.configAndResult))

      React.Fragment(
        <.div(ExploreStyles.ModesTableTitle)(
          <.label(
            ExploreStyles.ModesTableCount,
            s"${rows.length} available configurations, ${selectedCount} selected",
            HelpIcon("configuration/imaging_table.md".refined)
          ),
          <.div(
            ExploreStyles.ModesTableInfo,
            errlabel.toTagMod,
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
                ExploreStyles.TableRowSelected.when:
                  props.selectedConfigs.get.contains(row.original.entry.instrument)
                ,
                (^.onClick            ==> clickHandler(row.original)).when(row.original.entry.enabled)
              ),
            onChange = tableOnChangeHandler(visibleRows, atTop),
            virtualizerRef = virtualizerRef,
            emptyMessage = <.div(ExploreStyles.SpectroscopyTableEmpty, "No matching modes")
          ),
          scrollUpButton(
            upIndex,
            virtualizerRef,
            visibleRows.get,
            atTop.get
          ),
          scrollDownButton(
            downIndex,
            virtualizerRef,
            visibleRows.get
          )
        )
      )
