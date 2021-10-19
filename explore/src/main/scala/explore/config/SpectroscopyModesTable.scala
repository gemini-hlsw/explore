// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.data._
import cats.syntax.all._
import cats.effect.Sync
import cats.effect.std.Dispatcher
import coulomb.Quantity
import coulomb.refined._
import clue.data.syntax._
import clue.TransactionalClient
import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.Icons
import explore.AppCtx
import explore.View
import explore.common.ObsQueries._
import explore.common.ITCQueriesGQL._
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.schemas.ITC
import explore.implicits._
import explore.schemas.itcschema.implicits._
import explore.modes._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.FocalPlane
import lucuma.core.enum._
import lucuma.core.math.Wavelength
import lucuma.core.math.units.Micrometer
import lucuma.core.util.Display
import lucuma.core.model.SpatialProfile
import lucuma.core.model.SpectralDistribution
import lucuma.ui.reusability._
import react.common._
import react.common.implicits._
import react.semanticui.collections.table._
import react.semanticui.elements.button.Button
import react.virtuoso._
import react.virtuoso.raw.ListRange
import reactST.reactTable._
import reactST.reactTable.mod.DefaultSortTypes
import reactST.reactTable.mod.Row
import reactST.reactTable.util._
import spire.math.Bounded
import spire.math.Interval

import java.text.DecimalFormat
import lucuma.core.math.Angle
import explore.model.ConstraintSet
import explore.model.AirMassRange
import lucuma.core.model.Magnitude
import lucuma.core.math.MagnitudeValue
import scala.concurrent.duration._
import clue.data.Input
import monocle.Focus
import cats.Parallel

final case class SpectroscopyModesTable(
  scienceConfiguration:     View[Option[ScienceConfigurationData]],
  matrix:                   SpectroscopyModesMatrix,
  spectroscopyRequirements: SpectroscopyRequirementsData
) extends ReactFnProps[SpectroscopyModesTable](SpectroscopyModesTable.component)

sealed trait ItcQueryProblems

object ItcQueryProblems {
  case object UnsupportedMode          extends ItcQueryProblems
  case object MissingWavelength        extends ItcQueryProblems
  case object MissingSignalToNoise     extends ItcQueryProblems
  case class GenericError(msg: String) extends ItcQueryProblems
}

sealed trait ItcResult

object ItcResult {
  case object SourceTooBright                                     extends ItcResult
  case object Pending                                             extends ItcResult
  case class Result(exposureTime: FiniteDuration, exposures: Int) extends ItcResult
}

final case class ItcResultsCache(
  cache: Map[ItcResultsCache.CacheKey, EitherNec[ItcQueryProblems, ItcResult]]
) {
  import ItcResultsCache._

  def wavelength(w: Option[Wavelength]): EitherNec[ItcQueryProblems, Wavelength] =
    Either.fromOption(w, NonEmptyChain.of(ItcQueryProblems.MissingWavelength))

  def signalToNoise(w: Option[PosBigDecimal]): EitherNec[ItcQueryProblems, PosBigDecimal] =
    Either.fromOption(w, NonEmptyChain.of(ItcQueryProblems.MissingSignalToNoise))

  def mode(r: SpectroscopyModeRow): EitherNec[ItcQueryProblems, InstrumentModes] =
    Either.fromOption(r.toMode, NonEmptyChain.of(ItcQueryProblems.UnsupportedMode))

  def forRow(
    w:  Option[Wavelength],
    sn: Option[PosBigDecimal],
    r:  SpectroscopyModeRow
  ): EitherNec[ItcQueryProblems, ItcResult] =
    (wavelength(w), signalToNoise(sn), mode(r)).parMapN { (w, sn, im) =>
      cache.get((w, sn, im)).getOrElse(ItcResult.Pending.rightNec[ItcQueryProblems])
    }.flatten
}

object ItcResultsCache {
  type CacheKey = (Wavelength, PosBigDecimal, InstrumentModes)

  implicit class Row2Modes(val r: SpectroscopyModeRow) extends AnyVal {
    def toMode: Option[InstrumentModes] = r.instrument match {
      case GmosNorthSpectroscopyRow(d, f, fi) =>
        (new InstrumentModes(
          new GmosNITCInput(d, f, Input.orIgnore(fi)).assign
        )).some
      case _                                  => none
    }
  }

  val cache = Focus[ItcResultsCache](_.cache)

}

object SpectroscopyModesTable {
  type Props = SpectroscopyModesTable

  type ColId = NonEmptyString

  implicit val reuseProps: Reusability[Props] =
    Reusability.by(x => (x.scienceConfiguration, x.spectroscopyRequirements))

  implicit val listRangeReuse: Reusability[ListRange] =
    Reusability.by(x => (x.startIndex.toInt, x.endIndex.toInt))

  implicit val itcReuse: Reusability[ItcResultsCache] =
    Reusability.byRef

  protected val ModesTableDef = TableDef[SpectroscopyModeRow].withSort.withBlockLayout

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
        .setCell { c =>
          println(s"varue ${c.value}")
          c.value match {
            case Left(_)                       => "M/V"
            case Right(ItcResult.Result(t, c)) =>
              println("AHA")
              s"$c/$t"
            case Right(ItcResult.Pending)      => s"Pending"
            case Right(_)                      => s"N/A"
          }
        }
        .setWidth(66)
        .setMinWidth(66)
        .setMaxWidth(66)
        .setSortType(DefaultSortTypes.number),
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

  def queryItc[F[_]: Parallel: Dispatcher: Sync: TransactionalClient[*[_], ITC]](
    wavelength:    Wavelength,
    signalToNoise: PosBigDecimal,
    modes:         List[SpectroscopyModeRow],
    itcResults:    hooks.Hooks.UseState[ItcResultsCache]
  ) =
    modes
      .map(_.instrument)
      .collect { case m: GmosNorthSpectroscopyRow =>
        new InstrumentModes(m.toGmosNITCInput)
      }
      // ITC supports sending many modes at once, but sending them one by one
      // maximizes cache hits
      .parTraverse_(m =>
        SpectroscopyITCQuery
          .query(
            new ITCSpectroscopyInput(
              wavelength.toITCInput,
              signalToNoise,
              SpatialProfile.GaussianSource(Angle.fromDoubleArcseconds(10)),
              SpectralDistribution.Library(StellarLibrarySpectrum.A0I.asLeft),
              Magnitude(MagnitudeValue(6), MagnitudeBand.I, none, MagnitudeSystem.Vega).toITCInput,
              BigDecimal(0.1),
              ConstraintSet(
                ImageQuality.PointEight,
                CloudExtinction.PointFive,
                SkyBackground.Dark,
                WaterVapor.Dry,
                AirMassRange(
                  AirMassRange.DefaultMin,
                  AirMassRange.DefaultMax
                )
              ),
              List(m.assign)
            ).assign
          )
          .flatMap { x =>
            val update = x.spectroscopy.flatMap(_.results).map { r =>
              val im = new InstrumentModes(
                GmosNITCInput(r.mode.params.disperser,
                              r.mode.params.fpu,
                              r.mode.params.filter.orIgnore
                ).assign
              )
              val m  = r.itc match {
                case ItcError(m, _)   => ItcQueryProblems.GenericError(m).leftNec
                case ItcSuccess(e, t) => ItcResult.Result(t.microseconds.microseconds, e).rightNec
              }
              (wavelength, signalToNoise, im) -> m
            }
            itcResults
              .modState(ItcResultsCache.cache.modify(_ ++ update))
              .to[F]
          }
      )
      .runAsyncAndForgetCB

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
                coverage = s.wavelengthRange
                  .map(_.micrometer.toValue[BigDecimal].toRefined[Positive])
              )
          val (enabled, disabled) = rows.partition(enabledRow)
          (enabled ++ disabled)
        }
      )
      // itc results cache
      .useState(
        ItcResultsCache(Map.empty[ItcResultsCache.CacheKey, EitherNec[ItcQueryProblems, ItcResult]])
      )
      // cols
      .useMemoBy { (props, _, itc) => // Memo Cols
        (props.spectroscopyRequirements.wavelength,
         props.spectroscopyRequirements.focalPlane,
         props.spectroscopyRequirements.signalToNoise,
         itc.value
        )
      }((_, _, _) => { case (wavelength, focalPlane, sn, itc) =>
        columns(wavelength, focalPlane, sn, itc)
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
      // If useRef can be used here, I'm not figuring out how to do that.
      // This useMemo may be deceptive: it actually memoizes the ref, which is a wrapper to a mutable value.
      .useMemo(())(_ => ModesTable.createRef)
      // visibleRange
      .useState(none[ListRange])
      // atTop
      .useState(false)
      .render {
        (
          props,
          rows,
          itcResults,
          _,
          selectedIndex,
          tableInstance,
          virtuosoRef,
          visibleRange,
          atTop
        ) =>
          def toggleRow(row: SpectroscopyModeRow): Option[ScienceConfigurationData] =
            rowToConf(row).filterNot(conf => props.scienceConfiguration.get.contains_(conf))

          val s = visibleRange.value.map(_.startIndex).foldMap(_.toInt)
          val e = visibleRange.value.map(_.endIndex).foldMap(_.toInt)

          val visibleRows =
            (for { i <- s to e } yield Either.catchNonFatal(tableInstance.rows(i).original))
              .collect { case Right(m) => m }

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
              <.label(s"${rows.length} matching configurations",
                      HelpIcon("configuration/table.md")
              ),
              AppCtx.using { implicit ctx =>
                Button(onClick =
                  (props.spectroscopyRequirements.wavelength,
                   props.spectroscopyRequirements.signalToNoise
                  )
                    .mapN((w, sn) => queryItc(w, sn, visibleRows.toList, itcResults.withEffect))
                    .getOrEmpty
                )("Query ITC")
              }
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
