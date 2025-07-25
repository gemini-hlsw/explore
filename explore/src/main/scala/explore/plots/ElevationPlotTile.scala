// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.plots

import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.*
import explore.common.UserPreferencesQueries.*
import explore.components.HelpIcon
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ElevationPlotScheduling
import explore.model.GlobalPreferences
import explore.model.display.given
import explore.model.enums.PlotRange
import explore.model.enums.TimeDisplay
import explore.model.enums.Visible
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Site
import lucuma.core.enums.TimingWindowInclusion
import lucuma.core.math.BoundedInterval
import lucuma.core.model.CoordinatesAtVizTime
import lucuma.core.model.Semester
import lucuma.core.model.TimingWindow
import lucuma.core.model.User
import lucuma.core.syntax.display.*
import lucuma.react.common.ReactFnProps
import lucuma.react.datepicker.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.ToggleButton
import lucuma.refined.*
import lucuma.ui.LucumaStyles
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.table.TableIcons
import org.typelevel.cats.time.given
import spire.math.extras.interval.IntervalSeq

import java.time.*

object ElevationPlotTile:
  def apply(
    userId:            Option[User.Id],
    tileId:            NonEmptyString,
    plotData:          PlotData,
    site:              Option[Site],
    obsTime:           Option[Instant],
    obsDuration:       Option[Duration],
    timingWindows:     List[TimingWindow] = List.empty,
    globalPreferences: GlobalPreferences,
    emptyMessage:      String
  ): Tile[Unit] =
    Tile(
      tileId,
      "Elevation Plot",
      bodyClass = ExploreStyles.ElevationPlotTileBody
    )(_ =>
      userId
        .map: uid =>
          Body(
            uid,
            plotData,
            site,
            obsTime,
            obsDuration,
            timingWindows,
            globalPreferences,
            emptyMessage
          ): VdomNode
        .getOrElse:
          <.div(
            ExploreStyles.FullHeightWidth |+| LucumaStyles.HVCenter |+| ExploreStyles.EmptyTreeContent,
            <.div("Select a target")
          )
    )

  private case class Body(
    userId:            User.Id,
    plotData:          PlotData,
    site:              Option[Site],
    obsTime:           Option[Instant],
    obsDuration:       Option[Duration],
    timingWindows:     List[TimingWindow],
    globalPreferences: GlobalPreferences,
    emptyMessage:      String
  ) extends ReactFnProps(Body.component)

  private object Body:
    private type Props = Body

    private val component = ScalaFnComponent[Props]: props =>
      for
        ctx     <- useContext(AppContext.ctx)
        // Plot options, will be read from the user preferences
        options <- useStateView:
                     ObjectPlotOptions
                       .default(
                         props.site,
                         props.obsTime,
                         props.plotData.value.headOption.map(_._2.tracking)
                       )
                       .copy(
                         range = props.globalPreferences.elevationPlotRange,
                         timeDisplay = props.globalPreferences.elevationPlotTime,
                         showScheduling = props.globalPreferences.elevationPlotScheduling,
                         visiblePlots = List(
                           Option.when(
                             props.globalPreferences.elevationPlotElevationVisible
                           )(SeriesType.Elevation),
                           Option.when(
                             props.globalPreferences.elevationPlotParallacticAngleVisible
                           )(SeriesType.ParallacticAngle),
                           Option.when(
                             props.globalPreferences.elevationPlotSkyBrightnessVisible
                           )(SeriesType.SkyBrightness),
                           Option.when(
                             props.globalPreferences.elevationPlotLunarElevationVisible
                           )(SeriesType.LunarElevation)
                         ).flattenOption
                       )
        // If predefined site changes, switch to it.
        _       <- useEffectWithDeps(props.site):
                     _.map(options.zoom(ObjectPlotOptions.site).set).orEmpty
        // If visualization time changes, switch to it.
        _       <- useEffectWithDeps(props.obsTime):
                     _.map(ot => options.mod(_.withDateAndSemesterOf(ot))).orEmpty
        // If we select more than one observation, we cannot show a semester plot.
        _       <- useEffectWithDeps(props.plotData.value.size): selectedTargets =>
                     if selectedTargets > 1 && options.get.range === PlotRange.Semester then
                       options.mod(ObjectPlotOptions.range.replace(PlotRange.Night))
                     else Callback.empty
      yield
        import ctx.given

        val plotOptions: View[ObjectPlotOptions] = options.withOnMod: opts =>
          ElevationPlotPreference
            .updatePlotPreferences[IO](
              props.userId,
              opts.range,
              opts.timeDisplay,
              opts.showScheduling.value,
              Visible(opts.visiblePlots.contains_(SeriesType.Elevation)),
              Visible(opts.visiblePlots.contains_(SeriesType.ParallacticAngle)),
              Visible(opts.visiblePlots.contains_(SeriesType.SkyBrightness)),
              Visible(opts.visiblePlots.contains_(SeriesType.LunarElevation))
            )
            .runAsync

        val opt: ObjectPlotOptions = plotOptions.get

        val siteView: View[Site]                              = plotOptions.zoom(ObjectPlotOptions.site)
        val rangeView: View[PlotRange]                        = plotOptions.zoom(ObjectPlotOptions.range)
        val dateView: View[LocalDate]                         = plotOptions.zoom(ObjectPlotOptions.date)
        val semesterView: View[Semester]                      = plotOptions.zoom(ObjectPlotOptions.semester)
        val timeDisplayView: View[TimeDisplay]                = plotOptions.zoom(ObjectPlotOptions.timeDisplay)
        val visiblePlotsView: View[List[SeriesType]]          =
          plotOptions.zoom(ObjectPlotOptions.visiblePlots)
        val showSchedulingView: View[ElevationPlotScheduling] =
          plotOptions.zoom(ObjectPlotOptions.showScheduling)

        def windowsToIntervals(windows: List[TimingWindow]): IntervalSeq[Instant] =
          windows
            .map(_.toIntervalSeq(opt.interval))
            .fold(IntervalSeq.empty[Instant])(_ | _)

        val windowsIntervalsParts =
          props.timingWindows
            .partition(_.inclusion === TimingWindowInclusion.Include)
            .toList
            .map(windowsToIntervals)

        val windowsNetIncludeIntervals: IntervalSeq[Instant] =
          // Intersection of "Include" intervals with the complement of "Exclude" intervals
          windowsIntervalsParts(0) & ~windowsIntervalsParts(1)

        val windowsNetExcludeIntervals: List[BoundedInterval[Instant]] =
          (props.timingWindows, showSchedulingView.get) match
            case (Nil, _)                         => Nil // No exclusions if no windows are defined.
            case (_, ElevationPlotScheduling.Off) => Nil
            case (_, _)                           =>
              (IntervalSeq(opt.interval) & ~windowsNetIncludeIntervals).intervals.toList
                .map(BoundedInterval.fromInterval)
                .flattenOption

        React.Fragment(
          HelpIcon("target/main/elevation-plot.md".refined, ExploreStyles.HelpIconFloating),
          <.div(ExploreStyles.ElevationPlot)(
            opt.range match
              case PlotRange.Night | PlotRange.FullDay =>
                NightPlot(
                  props.plotData,
                  dateView.get.atStartOfDay.toInstant(ZoneOffset.UTC),
                  windowsNetExcludeIntervals,
                  props.obsTime,
                  props.obsDuration,
                  plotOptions,
                  props.emptyMessage
                )
              case PlotRange.Semester                  =>
                props.plotData.value.headOption.map { case (_, data) =>
                  val coords: CoordinatesAtVizTime =
                    data.tracking
                      .at(semesterView.get.start.atSite(siteView.get).toInstant)
                      .getOrElse:
                        CoordinatesAtVizTime(data.tracking.baseCoordinates)

                  SemesterPlot(
                    plotOptions.get,
                    coords,
                    windowsNetExcludeIntervals
                  )
                }
          ),
          <.div(ExploreStyles.ElevationPlotControls)(
            SelectButtonEnumView(
              "elevation-plot-site".refined,
              siteView,
              buttonClass = LucumaPrimeStyles.Tiny |+| LucumaPrimeStyles.VeryCompact
            ),
            <.div(ExploreStyles.ElevationPlotDatePickerControls)(
              Button(
                onClick = opt.range match
                  case PlotRange.Night | PlotRange.FullDay => dateView.mod(_.minusDays(1))
                  // if we've run out of valid semesters, don't move
                  case PlotRange.Semester                  => semesterView.mod(s => s.prev.getOrElse(s))
                ,
                clazz = ExploreStyles.ElevationPlotDateButton,
                text = false,
                icon = Icons.ChevronLeftLight
              ).tiny.compact,
              opt.range match
                case PlotRange.Night | PlotRange.FullDay =>
                  Datepicker(
                    onChange = (newValue, _) => dateView.set(newValue.toLocalDateOpt.get)
                  )
                    .selected(opt.date.toJsDate)
                    .dateFormat("yyyy-MM-dd")
                    .className(ExploreStyles.ElevationPlotDateInput.htmlClass)
                case PlotRange.Semester                  =>
                  FormInputText(
                    id = "semester".refined,
                    value = opt.semester.longName,
                    inputClass = ExploreStyles.ElevationPlotDateInput
                  )(^.readOnly := true)
              ,
              Button(
                onClick = opt.range match
                  case PlotRange.Night | PlotRange.FullDay => dateView.mod(_.plusDays(1))
                  // if we've run out of valid semesters, don't move
                  case PlotRange.Semester                  => semesterView.mod(s => s.next.getOrElse(s))
                ,
                clazz = ExploreStyles.ElevationPlotDateButton,
                text = false,
                icon = TableIcons.ChevronRight
              ).tiny.compact
            ),
            SelectButtonEnumView(
              "elevation-plot-range".refined,
              rangeView,
              buttonClass = LucumaPrimeStyles.Tiny |+| LucumaPrimeStyles.VeryCompact,
              filterPred = value => value =!= PlotRange.Semester || props.plotData.value.size === 1
            ),
            SelectButtonMultipleEnumView(
              "elevation-plot-visible-series".refined,
              visiblePlotsView,
              buttonClass = LucumaPrimeStyles.Tiny |+| LucumaPrimeStyles.VeryCompact
            ).when: // Only show series selector if it's a night plot
              rangeView.get === PlotRange.Night
            ,
            SelectButtonEnumView(
              "elevation-plot-time".refined,
              timeDisplayView,
              buttonClass = LucumaPrimeStyles.Tiny |+| LucumaPrimeStyles.VeryCompact
            )(^.visibility.hidden.when(rangeView.when(_ === PlotRange.Semester))),
            ToggleButton(
              onLabel = "Scheduling: On",
              offLabel = "Scheduling: Off",
              checked = showSchedulingView.when(_.value),
              onChange = showSchedulingView.set.compose(ElevationPlotScheduling.Value.reverseGet),
              clazz = LucumaPrimeStyles.Tiny |+| LucumaPrimeStyles.VeryCompact
            )
          )
        )
