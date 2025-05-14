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
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.UserPreferencesQueries.TableStore
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.events.*
import explore.model.AppContext
import explore.model.InstrumentConfigAndItcResult
import explore.model.Progress
import explore.model.ScienceRequirements
import explore.model.ScienceRequirements.*
import explore.model.SupportedInstruments
import explore.model.WorkerClients.*
import explore.model.boopickle.*
import explore.model.boopickle.ItcPicklers.given
import explore.model.display.*
import explore.model.display.given
import explore.model.enums.ExposureTimeModeType
import explore.model.enums.TableId
import explore.model.enums.WavelengthUnits
import explore.model.itc.*
import explore.model.reusability.given
import explore.modes.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.hooks.Hooks.UseRef
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
import lucuma.core.util.NewBoolean
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.react.circularprogressbar.CircularProgressbar
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.react.floatingui.Placement
import lucuma.react.floatingui.syntax.*
import lucuma.react.primereact.Button
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.schemas.model.CentralWavelength
import lucuma.typed.tanstackVirtualCore as rawVirtual
import lucuma.ui.components.ThemeIcons
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import lucuma.ui.table.ColumnSize.*
import lucuma.ui.table.hooks.*
import lucuma.ui.utils.*

import java.text.DecimalFormat
import scala.collection.decorators.*
import scala.concurrent.duration.*
import scala.language.implicitConversions

import scalajs.js

case class SpectroscopyModesTable(
  userId:                   Option[User.Id],
  selectedConfig:           View[Option[InstrumentConfigAndItcResult]],
  spectroscopyRequirements: ScienceRequirements.Spectroscopy,
  constraints:              ConstraintSet,
  targets:                  Option[List[ItcTarget]],
  baseCoordinates:          Option[CoordinatesAtVizTime],
  matrix:                   SpectroscopyModesMatrix,
  customSedTimestamps:      List[Timestamp],
  units:                    WavelengthUnits
) extends ReactFnProps(SpectroscopyModesTable.component):
  val validTargets = targets.map(_.filter(_.canQueryITC)).flatMap(NonEmptyList.fromList)

private object SpectroscopyModesTable:
  private type Props = SpectroscopyModesTable

  private object ScrollTo extends NewBoolean:
    inline def Scroll = True; inline def NoScroll = False

  private enum TimeOrSNColumn:
    case Time, SN

  private given Reusability[SpectroscopyModeRow]     = Reusability.by(_.id)
  private given Reusability[SpectroscopyModesMatrix] = Reusability.by(_.matrix.length)
  private given Reusability[ItcResultsCache]         = Reusability.by(_.cache.size)

  private case class SpectroscopyModeRowWithResult(
    entry:                SpectroscopyModeRow,
    result:               Pot[EitherNec[ItcTargetProblem, ItcResult]],
    wavelengthInterval:   Option[BoundedInterval[Wavelength]],
    configurationSummary: Option[String]
  ):
    lazy val rowId: RowId = RowId(entry.id.orEmpty.toString)

    lazy val totalItcTime: Option[TimeSpan] =
      result.toOption
        .collect { case Right(ItcResult.Result(e, t, _, _)) => e *| t.value }

    lazy val totalSN: Option[SignalToNoise] =
      result.toOption.collect { case Right(ItcResult.Result(_, _, _, s)) =>
        s.map(_.total.value)
      }.flatten

    lazy val singleSN: Option[SignalToNoise] =
      result.toOption.collect { case Right(ItcResult.Result(_, _, _, s)) =>
        s.map(_.single.value)
      }.flatten

  private case class TableMeta(itcProgress: Option[Progress])

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

  private def formatFilter(filter: ItcInstrumentConfig#Filter): String = filter match
    case Some(f: GmosSouthFilter) => f.shortName
    case Some(f: GmosNorthFilter) => f.shortName
    case f: Flamingos2Filter      => f.shortName
    case f: GnirsFilter           => f.shortName
    case _: None.type             => "none"
    case r                        => r.toString

  // I think these are valid Orderings because they should be consistent with ==
  // They could probably be Orders, as well, but only Ordering is actually needed here.
  private given Ordering[ItcInstrumentConfig#Filter] = Ordering.by(_.toString)

  private def formatInstrument(r: (Instrument, NonEmptyString)): String = r match
    case (i @ Instrument.Gnirs, m) => s"${i.longName} $m"
    case (i, _)                    => i.longName

  private def formatFPU(r: FocalPlane): String = r match
    case FocalPlane.SingleSlit   => "Single"
    case FocalPlane.MultipleSlit => "Multi"
    case FocalPlane.IFU          => "IFU"

  private def itcCell(
    c:   Pot[EitherNec[ItcTargetProblem, ItcResult]],
    col: TimeOrSNColumn
  ): VdomElement = {
    val content: TagMod = c.toOption match
      case Some(Left(errors))               =>
        if (errors.exists(_.problem === ItcQueryProblem.UnsupportedMode))
          <.span(Icons.Ban(^.color.red))
            .withTooltip(tooltip = "Mode not supported", placement = Placement.RightStart)
        else
          import ItcQueryProblem.*

          def renderName(name: Option[NonEmptyString]): String =
            name.fold("")(n => s"$n: ")

          val content: List[TagMod] =
            errors
              .collect:
                case ItcTargetProblem(name, s @ SourceTooBright(_)) =>
                  <.span(ThemeIcons.SunBright.addClass(ExploreStyles.ItcSourceTooBrightIcon))(
                    renderName(name) + (s: ItcQueryProblem).shortName
                  )
                case ItcTargetProblem(name, GenericError(e))        =>
                  e.split("\n")
                    .map(u => <.span(u))
                    .mkTagMod(<.span(renderName(name)), <.br, EmptyVdom)
                case ItcTargetProblem(name, problem)                =>
                  <.span(s"${renderName(name)}${problem.message}")
              .toList
              .intersperse(<.br: VdomNode)

          <.span(Icons.TriangleSolid.addClass(ExploreStyles.ItcErrorIcon))
            .withTooltip(tooltip = <.div(content.mkTagMod(<.span)), placement = Placement.RightEnd)
      case Some(Right(r: ItcResult.Result)) =>
        val content = col.match
          case TimeOrSNColumn.Time =>
            formatDurationHours(r.duration)
          case TimeOrSNColumn.SN   =>
            r.snAt.map(_.total.value).foldMap(formatSN)

        val tooltipText = col match
          case TimeOrSNColumn.Time =>
            s"${r.exposures} × ${formatDurationSeconds(r.exposureTime)}"
          case TimeOrSNColumn.SN   =>
            s"${r.snAt.map(_.single.value).foldMap(formatSN)} / exposure"

        <.span(content)
          .withTooltip(
            placement = Placement.RightStart,
            tooltip = tooltipText
          )
      case Some(Right(ItcResult.Pending))   =>
        Icons.Spinner.withSpin(true)
      case _                                =>
        "-"

    <.div(ExploreStyles.ITCCell, content)
  }

  private def progressingCellHeader(txt: String)(
    header: HeaderContext[?, ?, TableMeta, ?, ?, ?, ?]
  ) =
    <.div(ExploreStyles.ITCHeaderCell)(
      txt,
      header.table.options.meta
        .flatMap(_.itcProgress)
        .map(p =>
          CircularProgressbar(
            p.percentage.value.value,
            strokeWidth = 15,
            className = "explore-modes-table-itc-circular-progressbar"
          )
        )
    )

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
      column(FilterColumnId, row => SpectroscopyModeRow.filter.get(row.entry))
        .withCell(cell => formatFilter(cell.value))
        .withColumnSize(FixedSize(69.toPx))
        .sortable,
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

  extension (row: SpectroscopyModeRow)
    private def enabledRow: Boolean =
      SupportedInstruments.contains_(row.instrument.instrument) &&
        row.focalPlane === FocalPlane.SingleSlit

  private val ScrollOptions =
    rawVirtual.mod
      .ScrollToOptions()
      .setBehavior(rawVirtual.mod.ScrollBehavior.smooth)
      .setAlign(rawVirtual.mod.ScrollAlignment.center)

  def scrollToVirtualizedIndex(
    selectedIndex:  Int,
    virtualizerRef: UseRef[Option[HTMLTableVirtualizer]]
  ): Callback =
    virtualizerRef.get.flatMap(refOpt =>
      Callback(refOpt.map(_.scrollToIndex(selectedIndex, ScrollOptions)))
    )

  private val component =
    ScalaFnComponent[Props]: props =>
      for {
        ctx         <- useContext(AppContext.ctx)
        itcResults  <- useState(ItcResultsCache.Empty)
        rows        <- useMemo(
                         (props.matrix,
                          props.spectroscopyRequirements,
                          props.baseCoordinates.map(_.value.dec),
                          itcResults.value,
                          props.validTargets,
                          props.constraints,
                          props.customSedTimestamps
                         )
                       ): (matrix, s, dec, itcResults, asterism, constraints, customSedTimestamps) =>

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

                         val sortedRows: List[SpectroscopyModeRow]    = rows.sortBy(_.enabledRow)
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
                           val result = (s.wavelength, asterism, s.exposureTimeMode).mapN {
                             (_, _, exposureMode) =>
                               itcResults.forRow(
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
                               row.wavelengthInterval(w),
                             row.instrument.shortName.some
                           )
        itcProgress <- useState(none[Progress])
        errs        <- useMemo(
                         (props.spectroscopyRequirements.wavelength,
                          props.spectroscopyRequirements.exposureTimeMode,
                          props.constraints,
                          rows,
                          itcResults.value
                         )
                       ): (_, _, _, rows, _) =>
                         rows.value
                           .map(_.result.toOption)
                           .collect:
                             case Some(Left(p)) =>
                               p.toList
                                 .filter:
                                   case e if e.problem === ItcQueryProblem.MissingTargetInfo => true
                                   case e if e.problem === ItcQueryProblem.MissingBrightness => true
                                   case _                                                    => false
                                 .distinct
                           .flatten
                           .toList
                           .distinct

        cols           <- useMemo((props.spectroscopyRequirements.modeType, props.units)): (m, u) =>
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
                                meta = TableMeta(itcProgress.value)
                              ),
                              TableStore(props.userId, TableId.SpectroscopyModes, cols)
                            )
        // We need to have an indicator of whether we need to scrollTo the selectedIndex as
        // a state because otherwise the scrollTo effect below would often run in the same "hook cyle"
        // as the index change, and it would use the old index so it would scroll to the wrong location.
        // By having it as state with the following `useEffectWithDepsBy`, the scrollTo effect will run
        // in the following "hook cycle" and get the proper index.
        scrollTo       <- useState(ScrollTo.Scroll)
        _              <- useEffectWithDeps(table.getState().sorting)(_ => scrollTo.setState(ScrollTo.Scroll))
        sortedRows     <- useMemo((rows, table.getState().sorting))(_ =>
                            table.getSortedRowModel().rows.map(_.original).toList
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
                            selectedRow.map(sRow => sortedRows.indexWhere(row => row.entry == sRow))
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
        visibleRows    <- useState(none[Range.Inclusive]) // visibleRows
        atTop          <- useState(false)                 // atTop
        // Recalculate ITC values if the wv or sn change or if the rows get modified
        itcValues      <-
          useEffectStreamResourceWithDeps(
            (props.spectroscopyRequirements.exposureTimeMode,
             props.constraints,
             props.validTargets,
             props.customSedTimestamps,
             rows.length
            )
          ): (expTimeMode, constraints, asterism, customSedTimestamps, _) =>
            import ctx.given

            (expTimeMode, expTimeMode.map(ExposureTimeMode.at.get), asterism)
              .mapN: (expTimeMode, snAt, asterism) =>
                val modes: List[SpectroscopyModeRowWithResult] =
                  sortedRows
                    .filterNot: row => // Discard modes already in the cache
                      val cache: Map[ItcRequestParams, EitherNec[ItcTargetProblem, ItcResult]] =
                        itcResults.value.cache

                      // center of the row range
                      val cw: Option[CentralWavelength] =
                        row.entry.intervalCenter(snAt)

                      cw.exists: _ =>
                        row.entry.instrument.instrument match
                          case i if SupportedInstruments.contains(i) =>
                            cache.contains:
                              ItcRequestParams(
                                expTimeMode,
                                constraints,
                                asterism,
                                customSedTimestamps,
                                row.entry.instrument
                              )
                          case _                                     => true

                Option.when(modes.nonEmpty):
                  val progressZero = Progress.initial(NonNegInt.unsafeFrom(modes.length)).some
                  for {
                    _       <- Resource.eval(itcProgress.setStateAsync(progressZero))
                    request <-
                      ItcClient[IO]
                        .request:
                          ItcMessage.Query(expTimeMode,
                                           constraints,
                                           asterism,
                                           customSedTimestamps,
                                           modes.map(_.entry)
                          )
                        .map:
                          // Avoid rerendering on every single result, it's slow.
                          _.groupWithin(100, 500.millis)
                            .evalMap: itcResponseChunk =>
                              itcProgress
                                .modStateAsync(
                                  _.map(
                                    _.increment(NonNegInt.unsafeFrom(itcResponseChunk.size))
                                  )
                                    .filterNot(_.complete)
                                ) >>
                                // Update the cache
                                itcResults.modStateAsync(_.updateN(itcResponseChunk.toList)) >>
                                // Enable scrolling to the selected row (which might have moved due to sorting)
                                scrollTo.setState(ScrollTo.Scroll).to[IO]
                            .onComplete(fs2.Stream.eval(itcProgress.setStateAsync(none)))
                  } yield request
              .flatten
              .orEmpty
        virtualizerRef <- useRef(none[HTMLTableVirtualizer])
        // scroll to the currently selected row.
        _              <- useEffectWithDeps((scrollTo, selectedIndex.value, rows)):
                            (scrollTo, selectedIndex, _) =>
                              Callback.when(scrollTo.value === ScrollTo.Scroll)(
                                selectedIndex.traverse_(scrollToVirtualizedIndex(_, virtualizerRef)) >>
                                  scrollTo.setState(ScrollTo.NoScroll)
                              )
      } yield

        def toggleRow(
          row: SpectroscopyModeRowWithResult
        ): Option[InstrumentConfigAndItcResult] =
          Option.when(
            props.selectedConfig.get.forall(_.instrumentConfig =!= row.entry.instrument)
          ):
            InstrumentConfigAndItcResult(row.entry.instrument, row.result.toOption)

        def scrollButton(content: VdomNode, style: Css, indexCondition: Int => Boolean): TagMod =
          selectedIndex.value.whenDefined(
            using
            idx =>
              Button(
                clazz = ExploreStyles.ScrollButton |+| style,
                severity = Button.Severity.Secondary,
                onClick = scrollToVirtualizedIndex(idx, virtualizerRef)
              ).withMods(content).compact.when(indexCondition(idx))
          )

        def renderName(name: Option[NonEmptyString]): String =
          name.fold("")(n => s"$n: ")

        val errLabel: List[VdomNode] =
          errs.collect:
            case ItcTargetProblem(name, ItcQueryProblem.MissingWavelength)       =>
              <.label(ExploreStyles.WarningLabel)(s"${renderName(name)}Set Wavelength")
            case ItcTargetProblem(name, ItcQueryProblem.MissingExposureTimeMode) =>
              <.label(ExploreStyles.WarningLabel)(s"${renderName(name)}Set Exposure time mode")
            case ItcTargetProblem(name, ItcQueryProblem.MissingTargetInfo)
                if props.spectroscopyRequirements.wavelength.isDefined =>
              <.label(ExploreStyles.WarningLabel)(s"${renderName(name)}Missing Target Info")
            case ItcTargetProblem(name, ItcQueryProblem.MissingBrightness)       =>
              <.label(ExploreStyles.WarningLabel)(s"${renderName(name)}No Brightness Defined")

        val selectedTarget: Option[VdomNode] =
          rows.value
            .map(_.result.toOption)
            .collect:
              case Some(Right(result @ ItcResult.Result(_, _, _, _))) =>
                result
            // Very short exposure times may have ambiguity WRT the brightest target.
            .maxByOption(result => (result.exposureTime, result.exposures))
            .flatMap(_.brightestIndex)
            .flatMap(brightestIndex => props.validTargets.flatMap(_.get(brightestIndex)))
            .map(t => <.label(ExploreStyles.ModesTableTarget)(s"on ${t.name.value}"))

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
                  ^.disabled := !row.original.entry.enabledRow,
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
                    .when(row.original.entry.enabledRow)
                ),
              onChange = virtualizer =>
                visibleRows.setState(
                  virtualizer
                    .getVirtualItems()
                    .some
                    .filter(_.nonEmpty)
                    .map(items => items.head.index.toInt to items.last.index.toInt)
                ) >> atTop.setState(virtualizer.scrollElement.scrollTop < 32),
              virtualizerRef = virtualizerRef,
              emptyMessage = <.div(ExploreStyles.SpectroscopyTableEmpty, "No matching modes")
            ),
            scrollButton(
              Icons.ChevronDoubleUp,
              ExploreStyles.SelectedUp,
              idx => !(idx === 0 && atTop.value) && visibleRows.value.exists(_.start + 1 > idx)
            ),
            scrollButton(
              Icons.ChevronDoubleDown,
              ExploreStyles.SelectedDown,
              idx => visibleRows.value.exists(_.end - 2 < idx)
            )
          )
        )
