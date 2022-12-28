// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import boopickle.DefaultBasic.*
import cats.Eq
import cats.Order
import cats.data.*
import cats.effect.*
import cats.effect.std.UUIDGen
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all.*
import coulomb.Quantity
import coulomb.policy.spire.standard.given
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.implicits.*
import crystal.react.reuse.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.UserPreferencesQueries.TableStore
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.events.*
import explore.itc.*
import explore.model.AppContext
import explore.model.CoordinatesAtVizTime
import explore.model.Progress
import explore.model.ScienceMode
import explore.model.ScienceModeAdvanced
import explore.model.ScienceModeBasic
import explore.model.WorkerClients.*
import explore.model.boopickle.Boopickle.*
import explore.model.boopickle.ItcPicklers.given
import explore.model.boopickle.*
import explore.model.enums.TableId
import explore.model.itc.ItcTarget
import explore.model.itc.*
import explore.model.reusability.given
import explore.modes.*
import explore.syntax.ui.*
import explore.syntax.ui.*
import explore.syntax.ui.given
import explore.utils.*
import japgolly.scalajs.react.Reusability.apply
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.FocalPlane
import lucuma.core.enums.*
import lucuma.core.math.Coordinates
import lucuma.core.math.Wavelength
import lucuma.core.math.units.Micrometer
import lucuma.core.model.ConstraintSet
import lucuma.core.model.SiderealTracking
import lucuma.core.model.User
import lucuma.core.util.Display
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.ColumnSize.*
import lucuma.ui.table.TableHooks
import lucuma.ui.table.*
import lucuma.utils.*
import queries.schemas.odb.ObsQueries.*
import react.circularprogressbar.CircularProgressbar
import react.common.Css
import react.common.ReactFnProps
import react.floatingui.Placement
import react.floatingui.syntax.*
import react.primereact.Button
import reactST.{tanstackTableCore => raw}
import reactST.{tanstackVirtualCore => rawVirtual}
import spire.math.Bounded
import spire.math.Interval
import scala.collection.decorators.*

import java.text.DecimalFormat
import java.util.UUID
import scala.concurrent.duration.*

import scalajs.js
import scalajs.js.JSConverters.*

case class SpectroscopyModesTable(
  userId:                   Option[User.Id],
  scienceMode:              View[Option[ScienceMode]],
  spectroscopyRequirements: SpectroscopyRequirementsData,
  constraints:              ConstraintSet,
  targets:                  Option[List[ItcTarget]],
  baseCoordinates:          Option[CoordinatesAtVizTime],
  matrix:                   SpectroscopyModesMatrix,
  onSelect:                 Callback
) extends ReactFnProps(SpectroscopyModesTable.component):
  val brightestTarget: Option[ItcTarget] =
    for
      w  <- spectroscopyRequirements.wavelength
      tg <- targets
      b  <- tg.brightestAt(w)
    yield b

private object SpectroscopyModesTable extends TableHooks:
  private type Props = SpectroscopyModesTable

  private given Reusability[EitherNec[ItcQueryProblems, ItcResult]]                        = Reusability.byEq
  private given Reusability[SpectroscopyModesMatrix]                                       = Reusability.by(_.matrix.length)
  private given Reusability[ItcResultsCache]                                               = Reusability.by(_.cache.size)
  private given Reusability[Map[ItcRequestParams, EitherNec[ItcQueryProblems, ItcResult]]] =
    Reusability.never

  private given Eq[Range.Inclusive]          = Eq.by(x => (x.start, x.end, x.step))
  private given Reusability[Range.Inclusive] = Reusability.byEq

  private case class SpectroscopyModeRowWithResult(
    entry:  SpectroscopyModeRow,
    result: EitherNec[ItcQueryProblems, ItcResult]
  ):
    lazy val exposureTime: Option[Int] =
      result.toOption.collect { case ItcResult.Result(e, t) => e.toMillis.toInt * t }

  private val ColDef = ColumnDef[SpectroscopyModeRowWithResult]

  private val decFormat = new DecimalFormat("0.###")

  private val gratingDisplay: Display[ModeGrating] = Display.byShortName {
    case ModeGrating.NoGrating      => "-"
    case ModeGrating.SomeGrating(t) => t
  }

  private def column[V](
    id:       ColumnId,
    accessor: SpectroscopyModeRowWithResult => V
  ): ColumnDef.Single[SpectroscopyModeRowWithResult, V] =
    ColDef(id, accessor, columnNames.getOrElse(id, id.value))

  private val SelectedColumnId: ColumnId    = ColumnId("selected")
  private val InstrumentColumnId: ColumnId  = ColumnId("instrument")
  private val SlitWidthColumnId: ColumnId   = ColumnId("slit_width")
  private val SlitLengthColumnId: ColumnId  = ColumnId("slit_length")
  private val GratingColumnId: ColumnId     = ColumnId("grating")
  private val FilterColumnId: ColumnId      = ColumnId("filter")
  private val CoverageColumnId: ColumnId    = ColumnId("coverage")
  private val FPUColumnId: ColumnId         = ColumnId("fpu")
  private val ResolutionColumnId: ColumnId  = ColumnId("resolution")
  private val AvailablityColumnId: ColumnId = ColumnId("availability")
  private val TimeColumnId: ColumnId        = ColumnId("time")

  private val columnNames: Map[ColumnId, String] =
    Map[ColumnId, String](
      InstrumentColumnId  -> "Instrument",
      SlitWidthColumnId   -> "Slit Width",
      SlitLengthColumnId  -> "Slit Length",
      GratingColumnId     -> "Grating",
      FilterColumnId      -> "Filter",
      FPUColumnId         -> "FPU",
      CoverageColumnId    -> "Coverage",
      ResolutionColumnId  -> "λ / Δλ",
      AvailablityColumnId -> "Avail.",
      TimeColumnId        -> "Time"
    )

  private val formatSlitWidth: ModeSlitSize => String = ss =>
    decFormat.format(
      ModeSlitSize.milliarcseconds.get(ss.size).setScale(3, BigDecimal.RoundingMode.UP)
    )

  private val formatSlitLength: ModeSlitSize => String = ss =>
    f"${ModeSlitSize.milliarcseconds.get(ss.size).setScale(0, BigDecimal.RoundingMode.DOWN)}%1.0f"

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

  given Order[InstrumentRow#Grating] = Order.by(_.toString)
  given Order[InstrumentRow#Filter]  = Order.by(_.toString)
  given Order[InstrumentRow#FPU]     = Order.by(_.toString)

  private def formatWavelengthCoverage(r: Interval[Quantity[BigDecimal, Micrometer]]): String =
    r match
      case Bounded(a, b, _) =>
        List(a, b)
          .map(q => decFormat.format(q.value.setScale(3, BigDecimal.RoundingMode.DOWN)))
          .mkString(" - ")
      case _                =>
        "-"

  private def formatInstrument(r: (Instrument, NonEmptyString)): String = r match
    case (i @ Instrument.Gnirs, m) => s"${i.longName} $m"
    case (i, _)                    => i.longName

  private def formatFPU(r: FocalPlane): String = r match
    case FocalPlane.SingleSlit   => "Single"
    case FocalPlane.MultipleSlit => "Multi"
    case FocalPlane.IFU          => "IFU"

  private def itcCell(c: EitherNec[ItcQueryProblems, ItcResult]): VdomElement = {
    val content: TagMod = c match
      case Left(nel)                        =>
        if (nel.exists(_ == ItcQueryProblems.UnsupportedMode))
          <.span(Icons.Ban.withColor("red"))
            .withTooltip(tooltip = "Mode not supported", placement = Placement.RightStart)
        else
          val content = nel
            .collect {
              case ItcQueryProblems.MissingSignalToNoise => <.span("Set S/N")
              case ItcQueryProblems.MissingWavelength    => <.span("Set Wavelength")
              case ItcQueryProblems.MissingTargetInfo    => <.span("Missing target info")
              case ItcQueryProblems.GenericError(e)      => e.split("\\.").mkTagMod(<.br)
            }
            .toList
            .intersperse(<.br: VdomNode)

          <.span(Icons.TriangleSolid)
            .withTooltip(tooltip = <.div(content.mkTagMod(<.span)), placement = Placement.RightEnd)
      case Right(r: ItcResult.Result)       =>
        <.span(formatDuration(r.duration.toSeconds))
          .withTooltip(
            placement = Placement.RightStart,
            tooltip = s"${r.exposures} × ${formatDuration(r.exposureTime.toSeconds)}"
          )
      case Right(ItcResult.Pending)         =>
        Icons.Spinner.withSpin(true)
      case Right(ItcResult.SourceTooBright) =>
        <.span(Icons.SunBright.withColor("yellow"))
          .withTooltip(tooltip = "Source too bright")

    <.div(ExploreStyles.ITCCell, content)
  }

  private def columns(
    cw:          Option[Wavelength],
    fpu:         Option[FocalPlane],
    sn:          Option[PosBigDecimal],
    snAt:        Option[Wavelength],
    constraints: ConstraintSet,
    target:      Option[ItcTarget],
    itc:         ItcResultsCache,
    progress:    Option[Progress]
  ) =
    List(
      column(InstrumentColumnId, row => SpectroscopyModeRow.instrumentAndConfig.get(row.entry))
        .setCell(cell => formatInstrument(cell.value))
        .setColumnSize(Resizable(120.toPx, min = 50.toPx, max = 150.toPx)),
      column(TimeColumnId, _.exposureTime.orUndefined)
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
        .setCell(cell => itcCell(cell.row.original.result))
        .setColumnSize(FixedSize(80.toPx))
        .setEnableSorting(progress.isEmpty)
        .setSortUndefined(UndefinedPriority.Lower),
      column(SlitWidthColumnId, row => SpectroscopyModeRow.slitWidth.get(row.entry))
        .setCell(cell => formatSlitWidth(cell.value))
        .setColumnSize(FixedSize(100.toPx)),
      column(SlitLengthColumnId, row => SpectroscopyModeRow.slitLength.get(row.entry))
        .setCell(cell => formatSlitLength(cell.value))
        .setColumnSize(FixedSize(105.toPx)),
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
      column(CoverageColumnId, row => SpectroscopyModeRow.coverageInterval(cw)(row.entry))
        .setCell(cell => formatWavelengthCoverage(cell.value))
        .setColumnSize(FixedSize(100.toPx)),
      column(ResolutionColumnId, row => SpectroscopyModeRow.resolution.get(row.entry))
        .setCell(_.value.toString)
        .setColumnSize(FixedSize(70.toPx)),
      column(AvailablityColumnId, row => rowToConf(row.entry))
        .setCell(_.value.fold("No")(_ => "Yes"))
        .setColumnSize(FixedSize(66.toPx))
        .setSortUndefined(UndefinedPriority.Lower)
    ).filter { case c => (c.id.toString) != FPUColumnId.value || fpu.isEmpty }

  extension (row: SpectroscopyModeRow)
    def rowToConf: Option[ScienceMode] =
      row.instrument match
        case GmosNorthSpectroscopyRow(grating, fpu, filter)
            if row.focalPlane === FocalPlane.SingleSlit =>
          ScienceMode
            .GmosNorthLongSlit(
              basic = ScienceModeBasic.GmosNorthLongSlit(grating, filter, fpu),
              advanced = ScienceModeAdvanced.GmosNorthLongSlit.Empty
            )
            .some
        case GmosSouthSpectroscopyRow(grating, fpu, filter)
            if row.focalPlane === FocalPlane.SingleSlit =>
          ScienceMode
            .GmosSouthLongSlit(
              basic = ScienceModeBasic.GmosSouthLongSlit(grating, filter, fpu),
              advanced = ScienceModeAdvanced.GmosSouthLongSlit.Empty
            )
            .some
        case _ => none

    def equalsConf(conf: ScienceMode): Boolean =
      rowToConf.exists(_ === conf)

    def enabledRow: Boolean =
      List(Instrument.GmosNorth, Instrument.GmosSouth).contains_(row.instrument.instrument) &&
        row.focalPlane === FocalPlane.SingleSlit

  private def selectedRowIndex(
    scienceMode: Option[ScienceMode],
    rows:        List[SpectroscopyModeRowWithResult]
  ): Option[Int] =
    scienceMode
      .map(selected => rows.indexWhere(_.entry.equalsConf(selected)))
      .filterNot(_ === -1)

  private def getVisibleOriginalRows(
    visibleRange: Range,
    rows:         List[SpectroscopyModeRow]
  ): List[SpectroscopyModeRow] =
    (for i <- visibleRange.start to visibleRange.end
    yield rows.get(i)).toList.flattenOption

  private val ScrollOptions =
    rawVirtual.mod
      .ScrollToOptions()
      .setSmoothScroll(true)
      .setAlign(rawVirtual.mod.ScrollAlignment.center)

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
        val rows                =
          matrix
            .filtered(
              focalPlane = s.focalPlane,
              capabilities = s.capabilities,
              wavelength = s.wavelength,
              slitWidth = s.focalPlaneAngle,
              resolution = s.resolution,
              coverage = s.wavelengthCoverage.flatMap(
                _.micrometer.toValue[BigDecimal].toRefined[NonNegative].toOption
              ),
              declination = dec
            )
        val (enabled, disabled) = rows.partition(_.enabledRow)
        (enabled ++ disabled).map(row =>
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
      } { (_, _, _, _, _) => (wavelength, sn, snAt, targets, constraints, rows, itcResults) =>
        rows.value
          .map(_.result)
          .collect { case Left(p) =>
            p.toList.filter {
              case ItcQueryProblems.MissingTargetInfo => true
              case _                                  => false
            }.distinct
          }
          .flatten
          .toList
          .distinct
      }
      .useMemoBy { (props, _, itcResults, _, itcProgress, _) => // Memo Cols
        (props.spectroscopyRequirements.wavelength,
         props.spectroscopyRequirements.focalPlane,
         props.spectroscopyRequirements.signalToNoise,
         props.spectroscopyRequirements.signalToNoiseAt,
         props.brightestTarget,
         props.constraints,
         itcResults.value,
         itcProgress.value
        )
      } {
        (_, _, _, _, _, _) => (
          wavelength,
          focalPlane,
          sn,
          signalToNoiseAt,
          targets,
          constraints,
          itcResults,
          itcProgress
        ) =>
          columns(
            wavelength,
            focalPlane,
            sn,
            signalToNoiseAt,
            constraints,
            targets,
            itcResults,
            itcProgress
          )
      }
      // selectedIndex
      .useStateBy((props, _, _, rows, _, _, _) => selectedRowIndex(props.scienceMode.get, rows))
      // Recompute state if conf or requirements change.
      .useEffectWithDepsBy((props, _, _, _, _, _, _, _) =>
        (props.scienceMode.get, props.spectroscopyRequirements)
      ) { (_, _, _, rows, _, _, _, selectedIndex) => (scienceMode, _) =>
        selectedIndex.setState(selectedRowIndex(scienceMode, rows))
      }
      // table
      .useReactTableWithStateStoreBy((props, ctx, _, rows, _, _, cols, _) =>
        import ctx.given

        TableOptionsWithStateStore(
          TableOptions(
            cols,
            rows,
            getRowId = (row, _, _) => RowId(row.entry.id.toString),
            enableSorting = true
          ),
          TableStore(props.userId, TableId.SpectroscopyModes, cols)
        )
      )
      // visibleRows
      .useStateBy((_, _, _, rows, _, _, _, _, _) => none[Range.Inclusive])
      // atTop
      .useState(false)
      // Recalculate ITC values if the wv or sn change or if the rows get modified
      .useStreamResourceBy((props, _, _, _, _, _, _, _, _, visibleRows, _) =>
        (
          props.spectroscopyRequirements.wavelength,
          props.spectroscopyRequirements.signalToNoise,
          props.spectroscopyRequirements.signalToNoiseAt,
          props.constraints,
          props.brightestTarget
        )
      ) {
        (_, ctx, itcResults, _, itcProgress, _, _, _, table, visibleRows, _) => (
          wavelength,
          signalToNoise,
          signalToNoiseAt,
          constraints,
          brightestTarget
        ) =>
          import ctx.given

          (wavelength, signalToNoise, brightestTarget)
            .mapN { (w, sn, t) =>
              val sortedRows = table.getPreSortedRowModel().rows.map(_.original).toList

              val modes =
                sortedRows
                  .filterNot { row => // Discard modes already in the cache
                    val cache = itcResults.value.cache
                    val cw    = row.entry.coverageCenter(w)

                    cw.exists(w =>
                      row.entry.instrument.instrument match
                        case Instrument.GmosNorth | Instrument.GmosSouth =>
                          cache.contains(
                            ItcRequestParams(
                              w,
                              sn,
                              signalToNoiseAt,
                              constraints,
                              t,
                              row.entry.instrument
                            )
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
                        ItcMessage.Query(w, sn, constraints, t, modes.map(_.entry), signalToNoiseAt)
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
                              itcResults.modStateAsync(_.updateN(itcResponseChunk.toList))
                          )
                          .onComplete(fs2.Stream.eval(itcProgress.setStateAsync(none)))
                      )
                yield request).some
              } else none
            }
            .flatten
            .getOrElse(Resource.pure(fs2.Stream()))
      }
      .useRef(none[HTMLTableVirtualizer])
      .useEffectOnMountBy((_, _, _, _, _, _, _, selectedIndex, _, _, _, _, virtualizerRef) =>
        virtualizerRef.get.flatMap(refOpt =>
          Callback(
            for
              virtualizer <- refOpt
              idx         <- selectedIndex.value
            yield virtualizer.scrollToIndex(idx + 1, ScrollOptions)
          )
        )
      )
      .render {
        (
          props,
          _,
          _,
          rows,
          _,
          errs,
          _,
          selectedIndex,
          table,
          visibleRows,
          atTop,
          _,
          virtualizerRef
        ) =>
          def toggleRow(row: SpectroscopyModeRowWithResult): Option[ScienceMode] =
            rowToConf(row.entry).filterNot(conf => props.scienceMode.get.contains_(conf))

          def scrollButton(content: VdomNode, style: Css, indexCondition: Int => Boolean): TagMod =
            selectedIndex.value.whenDefined(idx =>
              Button(
                clazz = ExploreStyles.ScrollButton |+| style,
                severity = Button.Severity.Secondary,
                onClick = virtualizerRef.get.flatMap(ref =>
                  Callback(ref.foreach(_.scrollToIndex(idx + 1, ScrollOptions)))
                )
              ).withMods(content).compact.when(indexCondition(idx))
            )

          val errLabel: List[VdomNode] = errs
            .collect {
              case ItcQueryProblems.MissingWavelength    =>
                <.label(ExploreStyles.WarningLabel)("Set Wav..")
              case ItcQueryProblems.MissingSignalToNoise =>
                <.label(ExploreStyles.WarningLabel)("Set S/N")
              case ItcQueryProblems.MissingTargetInfo    =>
                <.label(ExploreStyles.WarningLabel)("Missing Target Info")
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
                      .when_(selectedIndex.value.exists(_ === row.index.toInt)),
                    (^.onClick --> (
                      props.scienceMode.set(toggleRow(row.original)) >>
                        selectedIndex.setState(row.index.toInt.some) >>
                        props.onSelect
                    )).when(row.original.entry.enabledRow)
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
