// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import boopickle.DefaultBasic.*
import cats.Eq
import cats.Order
import cats.data.*
import cats.effect.*
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all.*
import coulomb.Quantity
import crystal.react.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.UserPreferencesQueries
import explore.common.UserPreferencesQueries.TableStore
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.events.*
import explore.model.AppContext
import explore.model.BasicConfigAndItc
import explore.model.Progress
import explore.model.ScienceRequirements
import explore.model.WorkerClients.*
import explore.model.boopickle.*
import explore.model.boopickle.ItcPicklers.given
import explore.model.display.*
import explore.model.display.given
import explore.model.enums.TableId
import explore.model.itc.*
import explore.model.itc.ItcTarget
import explore.model.reusability.given
import explore.modes.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.hooks.Hooks.UseRef
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.enums.FocalPlane
import lucuma.core.math.*
import lucuma.core.model.*
import lucuma.core.syntax.all.*
import lucuma.core.util.NewType
import lucuma.core.util.TimeSpan
import lucuma.react.circularprogressbar.CircularProgressbar
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.react.floatingui.Placement
import lucuma.react.floatingui.syntax.*
import lucuma.react.primereact.Button
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.schemas.model.BasicConfiguration
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

import scalajs.js
import scalajs.js.JSConverters.*

case class SpectroscopyModesTable(
  userId:                   Option[User.Id],
  selectedConfig:           View[Option[BasicConfigAndItc]],
  spectroscopyRequirements: ScienceRequirements.Spectroscopy,
  constraints:              ConstraintSet,
  targets:                  Option[List[ItcTarget]],
  baseCoordinates:          Option[CoordinatesAtVizTime],
  matrix:                   SpectroscopyModesMatrix
) extends ReactFnProps(SpectroscopyModesTable.component):
  val brightestTarget: Option[ItcTarget] =
    for
      w <- spectroscopyRequirements.wavelength
      t <- targets.flatMap(_.map(_.gaiaFree).brightestProfileAt(_.profile)(w))
      if t.canQueryITC
    yield t

private object SpectroscopyModesTable:
  private type Props = SpectroscopyModesTable

  type ScrollTo = ScrollTo.Type
  object ScrollTo extends NewType[Boolean]:
    inline def Scroll   = ScrollTo(true)
    inline def NoScroll = ScrollTo(false)

  private given Reusability[SpectroscopyModeRow]     = Reusability.by(_.id)
  private given Reusability[SpectroscopyModesMatrix] = Reusability.by(_.matrix.length)
  private given Reusability[ItcResultsCache]         = Reusability.by(_.cache.size)

  private case class SpectroscopyModeRowWithResult(
    entry:  SpectroscopyModeRow,
    result: EitherNec[ItcQueryProblems, ItcResult]
  ):
    lazy val totalItcTime: Option[TimeSpan] =
      result.toOption.collect { case ItcResult.Result(e, t) => e *| t.value }

  private val ColDef = ColumnDef[SpectroscopyModeRowWithResult]

  private val decFormat = new DecimalFormat("0.###")

  private def column[V](
    id:       ColumnId,
    accessor: SpectroscopyModeRowWithResult => V
  ): ColumnDef.Single[SpectroscopyModeRowWithResult, V] =
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

  private val columnNames: Map[ColumnId, String] =
    Map[ColumnId, String](
      InstrumentColumnId         -> "Instrument",
      SlitWidthColumnId          -> "Slit Width",
      SlitLengthColumnId         -> "Slit Length",
      GratingColumnId            -> "Grating",
      FilterColumnId             -> "Filter",
      FPUColumnId                -> "FPU",
      WavelengthIntervalColumnId -> "λ Interval",
      ResolutionColumnId         -> "λ / Δλ",
      AvailablityColumnId        -> "Avail.",
      TimeColumnId               -> "Time"
    )

  private val formatSlitWidth: ModeSlitSize => String = ss =>
    decFormat.format(
      ModeSlitSize.milliarcseconds.get(ss.value).setScale(3, BigDecimal.RoundingMode.UP)
    )

  private val formatSlitLength: ModeSlitSize => String = ss =>
    f"${ModeSlitSize.milliarcseconds.get(ss.value).setScale(0, BigDecimal.RoundingMode.DOWN)}%1.0f"

  private def formatGrating(grating: InstrumentRow#Grating): String = grating match
    case f: GmosSouthGrating => f.shortName
    case f: GmosNorthGrating => f.shortName
    case f: F2Disperser      => f.shortName
    case f: GpiDisperser     => f.shortName
    case f: GnirsDisperser   => f.shortName
    case r                   => r.toString

  private def formatFilter(filter: InstrumentRow#Filter): String = filter match
    case Some(f: GmosSouthFilter) => f.shortName
    case Some(f: GmosNorthFilter) => f.shortName
    case f: F2Filter              => f.shortName
    case f: GnirsFilter           => f.shortName
    case _: None.type             => "none"
    case r                        => r.toString

  private given Order[InstrumentRow#Grating] = Order.by(_.toString)
  private given Order[InstrumentRow#Filter]  = Order.by(_.toString)
  private given Order[BasicConfigAndItc]     = Order.by(_.configuration.configurationSummary)
  private given Order[TimeSpan | Unit]       = Order.by(_.toOption)

  private def formatInstrument(r: (Instrument, NonEmptyString)): String = r match
    case (i @ Instrument.Gnirs, m) => s"${i.longName} $m"
    case (i, _)                    => i.longName

  private def formatFPU(r: FocalPlane): String = r match
    case FocalPlane.SingleSlit   => "Single"
    case FocalPlane.MultipleSlit => "Multi"
    case FocalPlane.IFU          => "IFU"

  private def itcCell(
    c: EitherNec[ItcQueryProblems, ItcResult],
    w: Option[Wavelength]
  ): VdomElement = {
    val content: TagMod = c match
      case Left(nel)                  =>
        if (nel.exists(_ == ItcQueryProblems.UnsupportedMode))
          <.span(Icons.Ban(^.color.red))
            .withTooltip(tooltip = "Mode not supported", placement = Placement.RightStart)
        else
          val content = nel
            .collect {
              case ItcQueryProblems.MissingSignalToNoise             => <.span("Set S/N")
              case ItcQueryProblems.MissingSignalToNoiseAt           =>
                <.span("Set Wavelength to measure S/N at")
              case ItcQueryProblems.MissingWavelength                => <.span("Set Wavelength")
              case ItcQueryProblems.MissingTargetInfo if w.isDefined =>
                <.span("Missing target info")
              case ItcQueryProblems.MissingBrightness                => <.span("No brightness defined")
              case s @ ItcQueryProblems.SourceTooBright(_)           =>
                <.span(ThemeIcons.SunBright.addClass(ExploreStyles.ItcSourceTooBrightIcon),
                       (s: ItcQueryProblems).shortName
                )
              case ItcQueryProblems.GenericError(e)                  =>
                e.split("\n").map(u => <.span(u)).intersperse(<.br: VdomNode).mkTagMod(<.span)
            }
            .toList
            .intersperse(<.br: VdomNode)

          <.span(Icons.TriangleSolid.addClass(ExploreStyles.ItcErrorIcon))
            .withTooltip(tooltip = <.div(content.mkTagMod(<.span)), placement = Placement.RightEnd)
      case Right(r: ItcResult.Result) =>
        <.span(formatDurationHours(r.duration))
          .withTooltip(
            placement = Placement.RightStart,
            tooltip = s"${r.exposures} × ${formatDurationSeconds(r.exposureTime)}"
          )
      case Right(ItcResult.Pending)   =>
        Icons.Spinner.withSpin(true)

    <.div(ExploreStyles.ITCCell, content)
  }

  private def columns(
    cw:                Option[Wavelength],
    fpu:               Option[FocalPlane],
    progress:          Option[Progress],
    timeSortDirection: Option[SortDirection]
  ) =
    List(
      column(
        InstrumentColumnId,
        row => formatInstrument(SpectroscopyModeRow.instrumentAndConfig.get(row.entry))
      )
        .setCell(_.value: String)
        .setColumnSize(Resizable(120.toPx, min = 50.toPx, max = 150.toPx))
        .sortable,
      column(TimeColumnId, _.totalItcTime.orUndefined)
        .setHeader(_ =>
          <.div(ExploreStyles.ITCHeaderCell)(
            "Time",
            progress
              .map(p =>
                CircularProgressbar(
                  p.percentage.value.value,
                  strokeWidth = 15,
                  className = "explore-modes-table-itc-circular-progressbar"
                )
              )
          )
        )
        .setCell(cell => itcCell(cell.row.original.result, cw))
        .setColumnSize(FixedSize(80.toPx))
        .setEnableSorting(progress.isEmpty)
        .setEnableSorting(true)
        .setSortUndefined {
          timeSortDirection match
            case Some(SortDirection.Descending) => UndefinedPriority.Higher
            case _                              => UndefinedPriority.Lower
        }
        .sortable,
      column(SlitWidthColumnId, row => SpectroscopyModeRow.slitWidth.get(row.entry))
        .setCell(cell => formatSlitWidth(cell.value.value))
        .setColumnSize(FixedSize(100.toPx))
        .sortable,
      column(SlitLengthColumnId, row => SpectroscopyModeRow.slitLength.get(row.entry))
        .setCell(cell => formatSlitLength(cell.value.value))
        .setColumnSize(FixedSize(105.toPx))
        .sortable,
      column(GratingColumnId, row => SpectroscopyModeRow.grating.get(row.entry))
        .setCell(cell => formatGrating(cell.value))
        .setColumnSize(FixedSize(96.toPx))
        .sortable,
      column(FilterColumnId, row => SpectroscopyModeRow.filter.get(row.entry))
        .setCell(cell => formatFilter(cell.value))
        .setColumnSize(FixedSize(69.toPx))
        .sortable,
      column(FPUColumnId, row => SpectroscopyModeRow.fpu.get(row.entry))
        .setCell(cell => formatFPU(cell.value))
        .setColumnSize(FixedSize(62.toPx))
        .sortable,
      column(
        WavelengthIntervalColumnId,
        row => cw.map(w => ModeCommonWavelengths.wavelengthInterval(w)(row.entry))
      ).setCell(cell => cell.value.flatten.fold("-")(_.shortName))
        .setColumnSize(FixedSize(100.toPx))
        .sortableBy(_.flatMap(_.map(_.lower))),
      column(ResolutionColumnId, row => SpectroscopyModeRow.resolution.get(row.entry))
        .setCell(_.value.toString)
        .setColumnSize(FixedSize(70.toPx))
        .sortable,
      column(AvailablityColumnId, row => row.rowToConf(cw))
        .setCell(_.value.fold("No")(_ => "Yes"))
        .setColumnSize(FixedSize(66.toPx))
        .sortable
    ).filter { case c => (c.id.toString) != FPUColumnId.value || fpu.isEmpty }

  extension (row: SpectroscopyModeRowWithResult)
    private def rowToConf(cw: Option[Wavelength]): Option[BasicConfigAndItc] =
      val config = cw.flatMap(row.entry.intervalCenter).flatMap { cc =>
        row.entry.instrument match
          case GmosNorthSpectroscopyRow(grating, fpu, filter, _)
              if row.entry.focalPlane === FocalPlane.SingleSlit =>
            BasicConfiguration
              .GmosNorthLongSlit(
                grating = grating,
                filter = filter,
                fpu = fpu,
                centralWavelength = cc
              )
              .some
          case GmosSouthSpectroscopyRow(grating, fpu, filter, _)
              if row.entry.focalPlane === FocalPlane.SingleSlit =>
            BasicConfiguration
              .GmosSouthLongSlit(
                grating = grating,
                filter = filter,
                fpu = fpu,
                centralWavelength = cc
              )
              .some
          case _ => none
      }
      config.map(c => BasicConfigAndItc(c, row.result.some))

    private def equalsConf(conf: BasicConfiguration, cw: Option[Wavelength]): Boolean =
      rowToConf(cw).exists(_.configuration === conf)

  extension (row: SpectroscopyModeRow)
    private def enabledRow: Boolean =
      List(Instrument.GmosNorth, Instrument.GmosSouth).contains_(row.instrument.instrument) &&
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
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      // itcResults
      .useState(
        ItcResultsCache(Map.empty[ItcRequestParams, EitherNec[ItcQueryProblems, ItcResult]])
      )
      // rows
      .useMemoBy((props, _, itcResults) =>
        (props.matrix,
         props.spectroscopyRequirements,
         props.baseCoordinates.map(_.value.dec),
         itcResults.value,
         props.brightestTarget,
         props.constraints
        )
      ) { (_, _, _) => (matrix, s, dec, itc, target, constraints) =>
        val rows       =
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
        val sortedRows = rows.sortBy(_.enabledRow)
        sortedRows.map(row =>
          SpectroscopyModeRowWithResult(
            row,
            itc.forRow(s.wavelength, s.signalToNoise, s.signalToNoiseAt, constraints, target, row)
          )
        )
      }
      // itcProgress
      .useState(none[Progress])
      .useMemoBy { (props, _, itcResults, rows, _) => // Calculate the common errors
        (props.spectroscopyRequirements.wavelength,
         props.spectroscopyRequirements.signalToNoise,
         props.spectroscopyRequirements.signalToNoiseAt,
         props.brightestTarget,
         props.constraints,
         rows,
         itcResults.value
        )
      } { (_, _, _, _, _) => (_, _, _, _, _, rows, _) =>
        rows.value
          .map(_.result)
          .collect { case Left(p) =>
            p.toList.filter {
              case ItcQueryProblems.MissingTargetInfo => true
              case ItcQueryProblems.MissingBrightness => true
              case _                                  => false
            }.distinct
          }
          .flatten
          .toList
          .distinct
      }
      // timeSortDescending: Time sort needs special treatment
      .useState(none[SortDirection])
      .useMemoBy { (props, _, itcResults, _, itcProgress, _, timeSortDirection) => // Memo Cols
        (props.spectroscopyRequirements.wavelength,
         props.spectroscopyRequirements.focalPlane,
         itcProgress.value,
         timeSortDirection.value
        )
      } {
        (_, _, _, _, _, _, _) => (
          wavelength,
          focalPlane,
          itcProgress,
          timeSortDirection
        ) =>
          columns(
            wavelength,
            focalPlane,
            itcProgress,
            timeSortDirection
          )
      }
      // table
      .useReactTableWithStateStoreBy((props, ctx, _, rows, _, _, _, cols) =>
        import ctx.given

        TableOptionsWithStateStore(
          TableOptions(
            cols,
            rows,
            getRowId = (row, _, _) => RowId(row.entry.id.value),
            enableSorting = true
          ),
          TableStore(props.userId, TableId.SpectroscopyModes, cols)
        )
      )
      // We need to have an indicator of whether we need to scrollTo the selectedIndex as
      // a state because otherwise the scrollTo effect below would often run in the same "hook cyle"
      // as the index change, and it would use the old index so it would scroll to the wrong location.
      // By having it as state with the following `useEffectWithDepsBy`, the scrollTo effect will run
      // in the following "hook cycle" and get the proper index.
      .useState(ScrollTo.Scroll)
      .useEffectWithDepsBy((_, _, _, _, _, _, _, _, table, _) => table.getState().sorting) {
        (_, _, _, _, _, _, timeSortDirection, _, table, scrollTo) => sorting =>
          table.setSorting(sorting) *> // Necessary to restore sorting after page reload
            timeSortDirection.setState(sorting.value.collectFirst {
              case ColumnSort(colId, direction) if colId == TimeColumnId => direction
            }) *>
            scrollTo.setState(ScrollTo.Scroll)
      }
      .useMemoBy((_, _, _, rows, _, _, timeSort, _, table, _) =>
        (rows, timeSort.value, table.getState().sorting)
      ) { (_, _, _, _, _, _, sorting, _, table, _) => (_, _, _) =>
        table.getSortedRowModel().rows.map(_.original).toList
      }
      // selectedRow
      .useState(none[SpectroscopyModeRow])
      // selectedIndex
      // The selected index needs to be the index into the sorted data, because that is what
      // the virtualizer uses for scrollTo.
      .useMemoBy((props, _, _, _, _, _, _, _, _, _, sortedRows, selectedRow) =>
        (sortedRows, selectedRow.value)
      )((_, _, _, _, _, _, _, _, _, _, _, _) =>
        (sortedRows, selectedRow) =>
          selectedRow.map(sRow => sortedRows.indexWhere(row => row.entry == sRow))
      )

      // Set the selected config if the rows change because it might have different itc data.
      // Note, we use rows for the dependency, not sorted rows, because sorted rows also changes with sort.
      .useEffectWithDepsBy((_, _, _, rows, _, _, _, _, _, _, _, _, _) => rows) {
        (props, _, _, _, _, _, _, _, _, _, sortedRows, _, selectedIndex) => _ =>
          val optRow = selectedIndex.value.flatMap(idx => sortedRows.lift(idx))
          val conf   = optRow.flatMap(_.rowToConf(props.spectroscopyRequirements.wavelength))
          if (props.selectedConfig.get =!= conf)
            props.selectedConfig.set(conf)
          else Callback.empty
      }
      // visibleRows
      .useState(none[Range.Inclusive])
      // atTop
      .useState(false)
      // Recalculate ITC values if the wv or sn change or if the rows get modified
      .useEffectWithDepsBy((props, _, _, rows, _, _, _, _, _, _, _, _, _, _, _) =>
        (
          props.spectroscopyRequirements.wavelength,
          props.spectroscopyRequirements.signalToNoise,
          props.spectroscopyRequirements.signalToNoiseAt,
          props.constraints,
          props.brightestTarget,
          rows.length
        )
      ) {
        (
          _,
          ctx,
          itcResults,
          _,
          itcProgress,
          _,
          _,
          _,
          _,
          scrollTo,
          sortedRows,
          _,
          _,
          _,
          _
        ) => (
          wavelength,
          signalToNoise,
          signalToNoiseAt,
          constraints,
          brightestTarget,
          _
        ) =>
          import ctx.given

          (wavelength, signalToNoise, signalToNoiseAt, brightestTarget)
            .mapN { (w, sn, snAt, t) =>
              val modes =
                sortedRows
                  .filterNot { row => // Discard modes already in the cache
                    val cache = itcResults.value.cache
                    val cw    = row.entry.intervalCenter(w)

                    cw.exists(w =>
                      row.entry.instrument.instrument match
                        case Instrument.GmosNorth | Instrument.GmosSouth =>
                          cache.contains(
                            ItcRequestParams(w, sn, snAt, constraints, t, row.entry.instrument)
                          )
                        case _                                           => true
                    )
                  }

              if (modes.nonEmpty) {
                val progressZero = Progress.initial(NonNegInt.unsafeFrom(modes.length)).some
                (for
                  _       <- Resource.eval(itcProgress.setStateAsync(progressZero))
                  request <-
                    ItcClient[IO]
                      .request(
                        ItcMessage.Query(w, sn, constraints, t, modes.map(_.entry), snAt)
                      )
                      .map(
                        // Avoid rerendering on every single result, it's slow.
                        _.groupWithin(100, 500.millis)
                          .evalTap(itcResponseChunk =>
                            itcProgress
                              .modStateAsync(
                                _.map(_.increment(NonNegInt.unsafeFrom(itcResponseChunk.size)))
                                  .filterNot(_.complete)
                              ) >>
                              // Update the cache
                              itcResults.modStateAsync(_.updateN(itcResponseChunk.toList)) >>
                              // Enable scrolling to the selected row (which might have moved due to sorting)
                              scrollTo.setState(ScrollTo.Scroll).to[IO]
                          )
                          .onComplete(fs2.Stream.eval(itcProgress.setStateAsync(none)))
                      )
                yield request).some
              } else none
            }
            .flatten
            .getOrElse(Resource.pure(fs2.Stream()))
            .use(_.compile.drain)
      }
      .useRef(none[HTMLTableVirtualizer])
      // scroll to the currently selected row.
      .useEffectWithDepsBy(
        (_, _, _, _, _, sortedRows, _, _, _, scrollTo, _, _, selectedIndex, _, _, _) =>
          (scrollTo, selectedIndex.value, sortedRows)
      ) {
        (
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          virtualizerRef
        ) => (scrollTo, selectedIndex, _) =>
          Callback.when(scrollTo.value === ScrollTo.Scroll)(
            selectedIndex.traverse_(scrollToVirtualizedIndex(_, virtualizerRef)) >>
              scrollTo.setState(ScrollTo.NoScroll)
          )
      }
      .render {
        (
          props,
          _,
          _,
          rows,
          _,
          errs,
          _,
          _,
          table,
          _,
          _,
          selectedRow,
          selectedIndex,
          visibleRows,
          atTop,
          virtualizerRef
        ) =>

          def toggleRow(
            row: SpectroscopyModeRowWithResult
          ): Option[explore.model.BasicConfigAndItc] =
            row
              .rowToConf(props.spectroscopyRequirements.wavelength)
              .filterNot(conf => props.selectedConfig.get.contains_(conf))

          def scrollButton(content: VdomNode, style: Css, indexCondition: Int => Boolean): TagMod =
            selectedIndex.value.whenDefined(idx =>
              Button(
                clazz = ExploreStyles.ScrollButton |+| style,
                severity = Button.Severity.Secondary,
                onClick = scrollToVirtualizedIndex(idx, virtualizerRef)
              ).withMods(content).compact.when(indexCondition(idx))
            )

          val errLabel: List[VdomNode] = errs
            .collect {
              case ItcQueryProblems.MissingWavelength      =>
                <.label(ExploreStyles.WarningLabel)("Set Wav..")
              case ItcQueryProblems.MissingSignalToNoise   =>
                <.label(ExploreStyles.WarningLabel)("Set S/N")
              case ItcQueryProblems.MissingSignalToNoiseAt =>
                <.label(ExploreStyles.WarningLabel)("Set S/N at")
              case ItcQueryProblems.MissingTargetInfo
                  if props.spectroscopyRequirements.wavelength.isDefined =>
                <.label(ExploreStyles.WarningLabel)("Missing Target Info")
              case ItcQueryProblems.MissingBrightness      =>
                <.label(ExploreStyles.WarningLabel)("No Brightness Defined")
            }

          val selectedTarget =
            for
              w <- props.spectroscopyRequirements.wavelength
              t <- props.brightestTarget
              if props.targets.exists(_.length > 1)
              if errLabel.isEmpty
            yield <.label(ExploreStyles.ModesTableTarget)(s"on ${t.name.value}").some

          React.Fragment(
            <.div(ExploreStyles.ModesTableTitle)(
              <.label(
                ExploreStyles.ModesTableCount,
                s"${rows.length} matching configurations",
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
                      .when(
                        props.selectedConfig.get.exists(c =>
                          row.original.equalsConf(c.configuration,
                                                  props.spectroscopyRequirements.wavelength
                          )
                        )
                      ),
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
      }
