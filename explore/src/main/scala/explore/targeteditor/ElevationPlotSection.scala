// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import explore.*
import explore.common.UserPreferencesQueries
import explore.common.UserPreferencesQueries.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ElevationPlotOptions
import explore.model.ElevationPlotScheduling
import explore.model.GlobalPreferences
import explore.model.display.given
import explore.model.enums.PlotRange
import explore.model.enums.TimeDisplay
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Site
import lucuma.core.enums.TimingWindowInclusion
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Coordinates
import lucuma.core.model.CoordinatesAtVizTime
import lucuma.core.model.ObjectTracking
import lucuma.core.model.Semester
import lucuma.core.model.Target
import lucuma.core.model.TimingWindow
import lucuma.core.model.User
import lucuma.core.syntax.display.given
import lucuma.react.common.ReactFnProps
import lucuma.react.datepicker.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.ToggleButton
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.LucumaPrimeStyles
import lucuma.ui.primereact.SelectButtonEnumView
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.table.TableIcons
import org.typelevel.cats.time.given
import spire.math.extras.interval.IntervalSeq

import java.time.*

case class ElevationPlotSection(
  userId:            User.Id,
  targetId:          Target.Id,
  tracking:          ObjectTracking,
  site:              Option[Site],
  visualizationTime: Option[Instant],
  pendingTime:       Option[Duration],
  timingWindows:     List[TimingWindow],
  globalPreferences: GlobalPreferences
) extends ReactFnProps(ElevationPlotSection.component)

object ElevationPlotSection:
  private type Props = ElevationPlotSection

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      // Plot options, will be read from the user preferences
      .useStateViewBy((props, _) =>
        ElevationPlotOptions
          .default(props.site, props.visualizationTime, props.tracking)
          .copy(
            range = props.globalPreferences.elevationPlotRange,
            timeDisplay = props.globalPreferences.elevationPlotTime,
            showScheduling = props.globalPreferences.elevationPlotScheduling,
            elevationPlotElevationVisible = props.globalPreferences.elevationPlotElevationVisible,
            elevationPlotParallacticAngleVisible =
              props.globalPreferences.elevationPlotParallacticAngleVisible,
            elevationPlotSkyBrightnessVisible =
              props.globalPreferences.elevationPlotSkyBrightnessVisible,
            elevationPlotLunarElevationVisible =
              props.globalPreferences.elevationPlotLunarElevationVisible
          )
      )
      // If predefined site changes, switch to it.
      .useEffectWithDepsBy((props, _, _) => props.site)((props, _, options) =>
        _.map(options.zoom(ElevationPlotOptions.site).set).orEmpty
      )
      // If visualization time changes, switch to it.
      .useEffectWithDepsBy((props, _, _) => props.visualizationTime)((props, _, options) =>
        _.map(vt => options.mod(_.withDateAndSemesterOf(vt))).orEmpty
      )
      .render { (props, ctx, elevationPlotOptions) =>
        import ctx.given

        val options = elevationPlotOptions.withOnMod(opts =>
          ElevationPlotPreference
            .updatePlotPreferences[IO](
              props.userId,
              opts.range,
              opts.timeDisplay,
              opts.showScheduling.value,
              opts.elevationPlotElevationVisible,
              opts.elevationPlotParallacticAngleVisible,
              opts.elevationPlotSkyBrightnessVisible,
              opts.elevationPlotLunarElevationVisible
            )
            .runAsync
        )

        val siteView: View[Site]                              = options.zoom(ElevationPlotOptions.site)
        val rangeView: View[PlotRange]                        = options.zoom(ElevationPlotOptions.range)
        val dateView: View[LocalDate]                         = options.zoom(ElevationPlotOptions.date)
        val semesterView: View[Semester]                      = options.zoom(ElevationPlotOptions.semester)
        val timeDisplayView: View[TimeDisplay]                = options.zoom(ElevationPlotOptions.timeDisplay)
        val showSchedulingView: View[ElevationPlotScheduling] =
          options.zoom(ElevationPlotOptions.showScheduling)

        val opt: ElevationPlotOptions = options.get

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
              case PlotRange.Night    =>
                val coords: CoordinatesAtVizTime =
                  props.tracking
                    .at(dateView.get.atStartOfDay.toInstant(ZoneOffset.UTC))
                    .getOrElse(CoordinatesAtVizTime(props.tracking.baseCoordinates))

                ElevationPlotNight(
                  coords,
                  props.visualizationTime,
                  windowsNetExcludeIntervals,
                  props.pendingTime,
                  options
                )
              case PlotRange.Semester =>
                val coords: CoordinatesAtVizTime =
                  props.tracking
                    .at(semesterView.get.start.atSite(siteView.get).toInstant)
                    .getOrElse(CoordinatesAtVizTime(props.tracking.baseCoordinates))

                ElevationPlotSemester(
                  options.get,
                  coords,
                  windowsNetExcludeIntervals
                ),
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
                  case PlotRange.Night    => dateView.mod(_.minusDays(1))
                  // if we've run out of valid semesters, don't move
                  case PlotRange.Semester => semesterView.mod(s => s.prev.getOrElse(s))
                ,
                clazz = ExploreStyles.ElevationPlotDateButton,
                text = false,
                icon = Icons.ChevronLeftLight
              ).tiny.compact,
              opt.range match
                case PlotRange.Night    =>
                  Datepicker(
                    onChange = (newValue, _) => dateView.set(newValue.toLocalDateOpt.get)
                  )
                    .selected(opt.date.toJsDate)
                    .dateFormat("yyyy-MM-dd")
                    .className(ExploreStyles.ElevationPlotDateInput.htmlClass)
                case PlotRange.Semester =>
                  FormInputText(
                    id = "semester".refined,
                    value = opt.semester.longName,
                    inputClass = ExploreStyles.ElevationPlotDateInput
                  )(^.readOnly := true)
              ,
              Button(
                onClick = opt.range match
                  case PlotRange.Night    => dateView.mod(_.plusDays(1))
                  // if we've run out of valid semesters, don't move
                  case PlotRange.Semester => semesterView.mod(s => s.next.getOrElse(s))
                ,
                clazz = ExploreStyles.ElevationPlotDateButton,
                text = false,
                icon = TableIcons.ChevronRight
              ).tiny.compact
            ),
            SelectButtonEnumView(
              "elevation-plot-range".refined,
              rangeView,
              buttonClass = LucumaPrimeStyles.Tiny |+| LucumaPrimeStyles.VeryCompact
            ),
            SelectButtonEnumView(
              "elevation-plot-time".refined,
              timeDisplayView,
              buttonClass = LucumaPrimeStyles.Tiny |+| LucumaPrimeStyles.VeryCompact
            )(^.visibility.hidden.when(rangeView.when(_ === PlotRange.Semester))),
            ToggleButton(
              onLabel = "Scheduling: On",
              offLabel = "Scheduling: Off",
              checked = showSchedulingView.when(_.value),
              onChange = showSchedulingView.set.compose(ElevationPlotScheduling.value.reverseGet),
              clazz = LucumaPrimeStyles.Tiny |+| LucumaPrimeStyles.VeryCompact
            )
          )
        )
      }
