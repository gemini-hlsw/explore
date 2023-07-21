// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import _root_.react.common.ReactFnProps
import _root_.react.datepicker.*
import _root_.react.primereact.Button
import _root_.react.primereact.ToggleButton
import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import explore.*
import explore.common.UserPreferencesQueries.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ElevationPlotOptions
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
import lucuma.core.model.Target
import lucuma.core.model.TimingWindow
import lucuma.core.model.User
import lucuma.core.syntax.display.given
import lucuma.refined.*
import lucuma.ui.primereact.LucumaPrimeStyles
import lucuma.ui.primereact.SelectButtonEnumView
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import org.typelevel.cats.time.given
import spire.math.extras.interval.IntervalSeq

import java.time.*

case class ElevationPlotSection(
  uid:               User.Id,
  tid:               Target.Id,
  site:              Option[Site],
  visualizationTime: Option[Instant],
  coords:            CoordinatesAtVizTime,
  timingWindows:     List[TimingWindow]
) extends ReactFnProps(ElevationPlotSection.component)

object ElevationPlotSection:
  private type Props = ElevationPlotSection

  private given Reusability[ElevationPlotOptions] = Reusability.byEq

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      // Plot options, will be read from the user preferences
      .useStateView(Pot.pending[ElevationPlotOptions])
      // If predefined site changes, switch to it.
      .useEffectWithDepsBy((props, _, _) => props.site)((props, _, options) =>
        _.map(options.zoom(Pot.readyPrism).zoom(ElevationPlotOptions.site).set).orEmpty
      )
      // If visualization time changes, switch to it.
      .useEffectWithDepsBy((props, _, _) => props.visualizationTime)((props, _, options) =>
        _.map(vt => options.zoom(Pot.readyPrism).mod(_.withDateAndSemesterOf(vt))).orEmpty
      )
      // Whenever options change, save them in user preferences
      .useEffectWithDepsBy((_, _, options) => options.get.toOption)((props, ctx, _) =>
        options =>
          import ctx.given

          options.map { opts =>
            ElevationPlotPreference
              .updatePlotPreferences[IO](props.uid, opts.range, opts.timeDisplay)
              .runAsync
              .void
          }.getOrEmpty
      )
      .useEffectWithDepsBy((props, _, _) => (props.uid, props.tid)) { (props, ctx, options) => _ =>
        import ctx.given

        ElevationPlotPreference
          .queryWithDefault[IO](props.uid)
          .flatMap { case (range, timeDisplay) =>
            options
              .set(
                ElevationPlotOptions
                  .default(props.site, props.visualizationTime, props.coords)
                  .copy(range = range, timeDisplay = timeDisplay)
                  .ready
              )
              .toAsync
          }
          .recover(_ =>
            ElevationPlotOptions.default(props.site, props.visualizationTime, props.coords)
          )
          .runAsyncAndForget
      }
      .render { (props, _, options) =>

        val siteView           = options.zoom(Pot.readyPrism.andThen(ElevationPlotOptions.site))
        val rangeView          = options.zoom(Pot.readyPrism.andThen(ElevationPlotOptions.range))
        val dateView           = options.zoom(Pot.readyPrism.andThen(ElevationPlotOptions.date))
        val semesterView       = options.zoom(Pot.readyPrism.andThen(ElevationPlotOptions.semester))
        val timeDisplayView    = options.zoom(Pot.readyPrism.andThen(ElevationPlotOptions.timeDisplay))
        val showSchedulingView =
          options.zoom(Pot.readyPrism.andThen(ElevationPlotOptions.showScheduling))

        options.renderPotView { opt =>

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
              case (Nil, _)                  => Nil // No exclusions if no windows are defined.
              case (_, Some(Visible.Hidden)) => Nil
              case (_, _)                    =>
                (IntervalSeq(opt.interval) & ~windowsNetIncludeIntervals).intervals.toList
                  .map(BoundedInterval.fromInterval)
                  .flattenOption

          <.div(ExploreStyles.ElevationPlotSection)(
            HelpIcon("target/main/elevation-plot.md".refined, ExploreStyles.HelpIconFloating),
            <.div(ExploreStyles.ElevationPlot) {
              opt.range match
                case PlotRange.Night    =>
                  ElevationPlotNight(
                    opt.site,
                    props.coords,
                    opt.date,
                    opt.timeDisplay,
                    props.visualizationTime,
                    windowsNetExcludeIntervals
                  )
                case PlotRange.Semester =>
                  val coords = props.coords
                  ElevationPlotSemester(
                    opt.site,
                    coords,
                    opt.semester,
                    opt.date,
                    windowsNetExcludeIntervals
                  )
            },
            <.div(
              ExploreStyles.ElevationPlotControls,
              siteView.asView.map(siteView =>
                SelectButtonEnumView(
                  "elevation-plot-site".refined,
                  siteView,
                  buttonClass = LucumaPrimeStyles.Tiny |+| LucumaPrimeStyles.VeryCompact
                )
              ),
              <.div(ExploreStyles.ElevationPlotDatePickerControls)(
                Button(
                  onClick = opt.range match
                    case PlotRange.Night    => dateView.mod(_.minusDays(1))
                    case PlotRange.Semester => semesterView.mod(_.prev)
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
                    case PlotRange.Semester => semesterView.mod(_.next)
                  ,
                  clazz = ExploreStyles.ElevationPlotDateButton,
                  text = false,
                  icon = Icons.ChevronRightLight
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
              )(^.visibility.hidden.when(rangeView.contains(PlotRange.Semester))),
              ToggleButton(
                onLabel = "Scheduling: On",
                offLabel = "Scheduling: Off",
                checked = showSchedulingView.get.getOrElse(Visible.Shown).isVisible,
                onChange = showSchedulingView.set.compose(Visible.value.reverseGet),
                clazz = LucumaPrimeStyles.Tiny |+| LucumaPrimeStyles.VeryCompact
              )
            )
          )
        }
      }
