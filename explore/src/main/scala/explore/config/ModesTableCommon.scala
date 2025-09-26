// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import boopickle.DefaultBasic.*
import cats.data.EitherNec
import cats.data.NonEmptyList
import cats.effect.*
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all.*
import crystal.Pot
import crystal.react.*
import crystal.react.hooks.useEffectStreamResourceWithDeps
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.events.ItcMessage
import explore.model.AppContext
import explore.model.InstrumentConfigAndItcResult
import explore.model.Progress
import explore.model.SupportedInstruments
import explore.model.WorkerClients.ItcClient
import explore.model.boopickle.*
import explore.model.boopickle.ItcPicklers.given
import explore.model.itc.*
import explore.model.reusability.given
import explore.modes.ItcInstrumentConfig
import japgolly.scalajs.react.*
import japgolly.scalajs.react.hooks.Hooks.UseRef
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.SignalToNoise
import lucuma.core.math.TotalSN
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.NewBoolean
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.react.circularprogressbar.CircularProgressbar
import lucuma.react.common.Css
import lucuma.react.fa.IconSize
import lucuma.react.floatingui.Placement
import lucuma.react.floatingui.syntax.*
import lucuma.react.primereact.Button
import lucuma.react.table.HTMLTableVirtualizer
import lucuma.react.table.HeaderContext
import lucuma.typed.tanstackVirtualCore as rawVirtual
import lucuma.ui.components.ThemeIcons
import lucuma.ui.format.*
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import workers.WorkerClient

import scala.collection.decorators.*
import scala.collection.immutable.SortedMap
import scala.concurrent.duration.*

trait ModesTableCommon:
  protected case class TableMeta(itcProgress: Option[Progress])

  extension (pot: Pot[EitherNec[ItcTargetProblem, ItcResult]])
    def totalItcTime: Option[TimeSpan] =
      pot.toOption.collect { case Right(ItcResult.Result(e, t, _, _, _)) => e *| t.value }

    def totalSN: Option[SignalToNoise] =
      pot.toOption.collect { case Right(ItcResult.Result(_, _, _, s, _)) =>
        s.map(_.total.value)
      }.flatten

  protected trait TableRowWithResult:
    val result: Pot[EitherNec[ItcTargetProblem, ItcResult]]
    val config: ItcInstrumentConfig
    lazy val configAndResult: InstrumentConfigAndItcResult =
      InstrumentConfigAndItcResult(config, result.toOption)

    lazy val totalItcTime: Option[TimeSpan] =
      result.toOption
        .collect { case Right(ItcResult.Result(e, t, _, _, _)) => e *| t.value }

    lazy val totalSN: Option[TotalSN] =
      result.toOption.collect { case Right(ItcResult.Result(_, _, _, s, _)) =>
        s.map(_.total)
      }.flatten

  protected object ScrollTo extends NewBoolean:
    inline def Scroll = True; inline def NoScroll = False
  protected type ScrollTo = ScrollTo.Type

  protected val ScrollOptions =
    rawVirtual.mod
      .ScrollToOptions()
      .setBehavior(rawVirtual.mod.ScrollBehavior.smooth)
      .setAlign(rawVirtual.mod.ScrollAlignment.center)

  protected def scrollToVirtualizedIndex(
    selectedIndex:  Int,
    virtualizerRef: UseRef[Option[HTMLTableVirtualizer]]
  ): Callback =
    virtualizerRef.get.flatMap(refOpt =>
      Callback(refOpt.map(_.scrollToIndex(selectedIndex, ScrollOptions)))
    )

  private def scrollButton(
    virtualizerRef: UseRef[Option[HTMLTableVirtualizer]],
    index:          Option[Int],
    content:        VdomNode,
    style:          Css,
    indexCondition: Int => Boolean
  ): TagMod =
    index.whenDefined(
      using
      idx =>
        Button(
          clazz = ExploreStyles.ScrollButton |+| style,
          severity = Button.Severity.Secondary,
          onClick = scrollToVirtualizedIndex(idx, virtualizerRef)
        ).withMods(content).compact.when(indexCondition(idx))
    )

  protected def scrollUpButton(
    index:          Option[Int],
    virtualizerRef: UseRef[Option[HTMLTableVirtualizer]],
    visibleRows:    Option[Range.Inclusive],
    atTop:          Boolean
  ): TagMod =
    scrollButton(
      virtualizerRef,
      index,
      Icons.ChevronDoubleUp,
      ExploreStyles.SelectedUp,
      idx => !(idx === 0 && atTop) && visibleRows.exists(_.start + 1 > idx)
    )

  protected def scrollDownButton(
    index:          Option[Int],
    virtualizerRef: UseRef[Option[HTMLTableVirtualizer]],
    visibleRows:    Option[Range.Inclusive]
  ): TagMod =
    scrollButton(virtualizerRef,
                 index,
                 Icons.ChevronDoubleDown,
                 ExploreStyles.SelectedDown,
                 idx => visibleRows.exists(_.end - 2 < idx)
    )

  protected def tableOnChangeHandler(
    visibleRows: View[Option[Range.Inclusive]],
    atTop:       View[Boolean]
  ): HTMLTableVirtualizer => Callback =
    virtualizer =>
      visibleRows.set(
        virtualizer
          .getVirtualItems()
          .some
          .filter(_.nonEmpty)
          .map(items => items.head.index.toInt to items.last.index.toInt)
      ) >> atTop.set(virtualizer.scrollElement.scrollTop < 32)

  protected enum ItcColumns:
    case Time, SN, Exposures

  protected def progressingCellHeader(txt: String)(
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

  def tooltipContent(
    baseText: String,
    warnings: SortedMap[Int, List[String]]
  ): (VdomNode, Placement) =
    if (warnings.nonEmpty) {
      (<.div(
         <.div(baseText),
         <.div("Warnings:"),
         warnings
           .map(w => <.div(ExploreStyles.WarningLabel, s"• CCD${w._1} ${w._2.mkString(", ")}"))
           .toVdomArray
       ),
       Placement.Bottom
      )
    } else {
      (baseText, Placement.RightStart)
    }

  protected def itcCell(
    c:   Pot[EitherNec[ItcTargetProblem, ItcResult]],
    col: ItcColumns
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
                case p @ ItcTargetProblem(name, s @ SourceTooBright(_)) =>
                  <.span(ThemeIcons.SunBright.addClass(ExploreStyles.ItcSourceTooBrightIcon))(
                    p.format
                  )
                case ItcTargetProblem(name, GenericError(e))            =>
                  e.split("\n")
                    .map(u => <.span(u))
                    .mkTagMod(<.span(renderName(name)), <.br, EmptyVdom)
                case p @ ItcTargetProblem(_, _)                         =>
                  <.span(p.format)
              .toList
              .intersperse(<.br: VdomNode)

          <.span(Icons.TriangleSolid.addClass(ExploreStyles.ItcErrorIcon))
            .withTooltip(tooltip = <.div(content.mkTagMod(<.span)), placement = Placement.RightEnd)
      case Some(Right(r: ItcResult.Result)) =>
        val ccdWarnings =
          r.ccdWarnings.collect:
            case a @ (_, v) if v.nonEmpty => a

        val content = col.match
          case ItcColumns.Exposures =>
            r.exposures.toString
          case ItcColumns.Time      =>
            formatDurationHours(r.duration)
          case ItcColumns.SN        =>
            r.snAt.map(_.total.value).foldMap(_.format)

        val (tooltip, placement) = col match
          case ItcColumns.Exposures =>
            ("": VdomNode, Placement.RightStart)
          case ItcColumns.Time      =>
            val baseText = s"${r.exposures} × ${formatDurationSeconds(r.exposureTime)}"
            tooltipContent(baseText, ccdWarnings)
          case ItcColumns.SN        =>
            val baseText = s"${r.snAt.map(_.single.value).foldMap(_.format)} / exposure"
            tooltipContent(baseText, ccdWarnings)

        (if (ccdWarnings.nonEmpty)
           <.span(
             content,
             Icons.ExclamationTriangle
               .withClass(ExploreStyles.WarningItcIcon)
               .withSize(IconSize.XS2)
           )
         else <.span(content))
          .withTooltip(
            placement = placement,
            tooltip = tooltip
          )
      case Some(Right(ItcResult.Pending))   =>
        Icons.Spinner.withSpin(true)
      case _                                =>
        "-"

    <.div(ExploreStyles.ITCCell, content)
  }

  protected case class ItcHookData(
    errors: List[ItcTargetProblem]
  ):
    def errorLabel(showMissingTargetInfo: Boolean): List[VdomNode] =
      def renderName3(name: Option[NonEmptyString]): String =
        name.fold("")(n => s"$n: ")
      errors.collect:
        case ItcTargetProblem(name, ItcQueryProblem.MissingWavelength)                          =>
          <.label(ExploreStyles.WarningLabel)(s"${renderName3(name)}Set Wavelength")
        case ItcTargetProblem(name, ItcQueryProblem.MissingExposureTimeMode)                    =>
          <.label(ExploreStyles.WarningLabel)(s"${renderName3(name)}Set Exposure time mode")
        case ItcTargetProblem(name, ItcQueryProblem.MissingTargetInfo) if showMissingTargetInfo =>
          <.label(ExploreStyles.WarningLabel)(s"${renderName3(name)}Missing Target Info")
        case ItcTargetProblem(name, ItcQueryProblem.MissingBrightness)                          =>
          <.label(ExploreStyles.WarningLabel)(s"${renderName3(name)}No Brightness Defined")

  protected given Reusability[ItcResultsCache] = Reusability.by(_.cache.size)

  protected def useItc[Row <: TableRowWithResult](
    itcResults:          View[ItcResultsCache],
    itcProgress:         View[Option[Progress]],
    onNewItc:            Callback,
    expTimeMode:         Option[ExposureTimeMode],
    constraints:         ConstraintSet,
    targets:             Option[NonEmptyList[ItcTarget]],
    customSedTimestamps: List[Timestamp],
    sortedRows:          Reusable[List[Row]]
  ): HookResult[ItcHookData] =
    for {
      ctx    <- useContext(AppContext.ctx)
      errors <- useMemo(sortedRows): rows =>
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
      _      <-
        useEffectStreamResourceWithDeps(
          (expTimeMode, constraints, targets, customSedTimestamps, sortedRows.length)
        ): (expTimeMode, constraints, asterism, customSedTimestamps, _) =>
          import ctx.given

          (expTimeMode, asterism)
            .mapN { (expTimeMode, asterism) =>
              val modes: List[Row] =
                sortedRows
                  .filterNot: row => // Discard modes already in the cache
                    val cache: Map[ItcRequestParams, EitherNec[ItcTargetProblem, ItcResult]] =
                      itcResults.get.cache

                    // The cache returns an error for unsupported instruments
                    row.config.instrument match
                      case i if SupportedInstruments.contains(i) =>
                        cache.contains:
                          ItcRequestParams(
                            expTimeMode,
                            constraints,
                            asterism,
                            customSedTimestamps,
                            row.config
                          )
                      case _                                     => true

              Option
                .when(modes.nonEmpty):
                  val progressZero = Progress.initial(NonNegInt.unsafeFrom(modes.length)).some
                  for {
                    _       <- Resource.eval(itcProgress.set(progressZero).to[IO])
                    request <-
                      ModesTableCommon
                        .requestItcQuery(
                          expTimeMode,
                          constraints,
                          asterism,
                          customSedTimestamps,
                          modes.map(_.config)
                        )
                        .map:
                          // Avoid rerendering on every single result, it's slow.
                          _.groupWithin(100, 500.millis)
                            .evalMap: itcResponseChunk =>
                              itcProgress
                                .mod(
                                  _.map(
                                    _.increment(NonNegInt.unsafeFrom(itcResponseChunk.size))
                                  )
                                    .filterNot(_.complete)
                                )
                                .to[IO] >>
                                // Update the cache
                                itcResults.mod(_.updateN(itcResponseChunk.toList)).to[IO] >>
                                // Enable scrolling to the selected row (which might have moved due to sorting)
                                onNewItc.to[IO]
                            .onComplete(fs2.Stream.eval(itcProgress.set(none).to[IO]))
                  } yield request
            }
            .flatten
            .orEmpty
    } yield ItcHookData(errors.value)

  def findSelectedTarget[Row <: TableRowWithResult](
    rows:         List[Row],
    validTargets: Option[NonEmptyList[ItcTarget]]
  ): Option[VdomNode] =
    rows
      .map(_.result.toOption)
      .collect:
        case Some(Right(result @ ItcResult.Result(_, _, _, _, _))) =>
          result
      // Very short exposure times may have ambiguity WRT the brightest target.
      .maxByOption(result => (result.exposureTime, result.exposures))
      .flatMap(_.brightestIndex)
      .flatMap(brightestIndex => validTargets.flatMap(_.get(brightestIndex)))
      .map(t => <.label(ExploreStyles.ModesTableTarget)(s"on ${t.name.value}"))

object ModesTableCommon:
  def requestItcQuery[F[_]](
    expTimeMode:         ExposureTimeMode,
    constraints:         ConstraintSet,
    asterism:            NonEmptyList[ItcTarget],
    customSedTimestamps: List[Timestamp],
    configs:             List[ItcInstrumentConfig]
  )(using
    WorkerClient[F, ItcMessage.Request]
  ): Resource[F, fs2.Stream[F, Map[ItcRequestParams, EitherNec[ItcTargetProblem, ItcResult]]]] =
    ItcClient[F].request(
      ItcMessage.Query(
        expTimeMode,
        constraints,
        asterism,
        customSedTimestamps,
        configs
      )
    )
