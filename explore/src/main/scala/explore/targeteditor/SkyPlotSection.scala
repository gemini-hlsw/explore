// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Eq
import cats.syntax.all._
import eu.timepit.refined.auto._
import explore._
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.ReactCats._
import japgolly.scalajs.react.ReactMonocle._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.Site
import lucuma.core.math.Coordinates
import lucuma.core.model.Semester
import lucuma.ui.reusability._
import monocle.Focus
import react.common.ReactProps
import react.datepicker._
import react.semanticui.elements.button.Button
import react.semanticui.elements.segment.Segment
import react.semanticui.modules.checkbox.Checkbox

import java.time.LocalDate
import java.time.ZoneId
import java.time.ZoneOffset
import java.time.ZonedDateTime

final case class SkyPlotSection(
  coords:           Coordinates
)(implicit val ctx: AppContextIO)
    extends ReactProps[SkyPlotSection](SkyPlotSection.component)

object SkyPlotSection {
  type Props = SkyPlotSection

  protected sealed trait PlotPeriod
  protected object PlotPeriod {
    final case object Night    extends PlotPeriod
    final case object Semester extends PlotPeriod

    implicit val PlotPeriodEq: Eq[PlotPeriod]             = Eq.fromUniversalEquals
    implicit val PlotPeriodReuse: Reusability[PlotPeriod] = Reusability.byEq
  }

  protected sealed trait TimeDisplay
  protected object TimeDisplay {
    final case object UTC  extends TimeDisplay
    final case object Site extends TimeDisplay

    implicit val TimeDisplayEq: Eq[TimeDisplay]             = Eq.fromUniversalEquals
    implicit val TimeDisplayReuse: Reusability[TimeDisplay] = Reusability.byEq
  }

  final case class State(
    site:        Site,
    date:        LocalDate,
    plotPeriod:  PlotPeriod,
    timeDisplay: TimeDisplay
  ) {
    def zoneId: ZoneId =
      timeDisplay match {
        case TimeDisplay.UTC  => ZoneOffset.UTC
        case TimeDisplay.Site => site.timezone
      }
  }

  object State {
    val site        = Focus[State](_.site)
    val date        = Focus[State](_.date)
    val plotPeriod  = Focus[State](_.plotPeriod)
    val timeDisplay = Focus[State](_.timeDisplay)
  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive
  implicit val stateReuse: Reusability[State] = Reusability.derive

  class Backend($ : BackendScope[Props, State]) {
    val toggleSite: Callback =
      $.modStateL(State.site) {
        case Site.GS => Site.GN
        case Site.GN => Site.GS
      }

    val togglePlotPeriod: Callback =
      $.modStateL(State.plotPeriod) {
        case PlotPeriod.Night    => PlotPeriod.Semester
        case PlotPeriod.Semester => PlotPeriod.Night
      }

    val toggleTimeDisplay: Callback =
      $.modStateL(State.timeDisplay) {
        case TimeDisplay.UTC  => TimeDisplay.Site
        case TimeDisplay.Site => TimeDisplay.UTC
      }

    def render(props: Props, state: State) = {
      implicit val ctx = props.ctx

      Segment(ExploreStyles.SkyPlotSection)(
        HelpIcon("target/main/elevation-plot.md", ExploreStyles.HelpIconFloating),
        <.div(ExploreStyles.SkyPlot) {
          state.plotPeriod match {
            case PlotPeriod.Night    =>
              SkyPlotNight(state.site, props.coords, state.date, state.zoneId, 350)
            case PlotPeriod.Semester =>
              val site     = state.site
              val coords   = props.coords
              val semester = Semester.fromLocalDate(state.date)
              val zoneId   = state.zoneId
              SkyPlotSemester(site, coords, semester, zoneId, 350).withKey(
                s"$site-$coords-$semester"
              )
          }
        },
        <.div(ExploreStyles.SkyPlotControls)(
          <.div(ExploreStyles.PlotToggleCheckbox)(
            <.label(Site.GN.toString,
                    ^.cursor.pointer,
                    ^.onClick --> $.setStateL(State.site)(Site.GN)
            ),
            Checkbox(slider = true,
                     clazz = ExploreStyles.PlotToggle,
                     onClick = toggleSite,
                     checked = state.site === Site.GS
            ),
            <.label(Site.GS.toString,
                    ^.cursor.pointer,
                    ^.onClick --> $.setStateL(State.site)(Site.GS)
            )
          ),
          <.div(ExploreStyles.SkyPlotDatePickerControls)(
            Button(onClick = $.modStateL(State.date)(_.minusDays(1)),
                   clazz = ExploreStyles.SkyPlotDateButton
            )(Icons.ChevronLeftLight),
            Datepicker(
              onChange = (newValue, _) => $.setStateL(State.date)(newValue.toLocalDateOpt.get)
            )
              .selected(state.date.toJsDate)
              .dateFormat("yyyy-MM-dd")
              .className(ExploreStyles.SkyPlotDatePicker.htmlClass),
            Button(onClick = $.modStateL(State.date)(_.plusDays(1)),
                   clazz = ExploreStyles.SkyPlotDateButton
            )(Icons.ChevronRightLight)
          ),
          <.div(ExploreStyles.PlotToggleCheckbox)(
            <.label(PlotPeriod.Night.toString,
                    ^.cursor.pointer,
                    ^.onClick --> $.setStateL(State.plotPeriod)(PlotPeriod.Night)
            ),
            Checkbox(slider = true,
                     clazz = ExploreStyles.PlotToggle,
                     onClick = togglePlotPeriod,
                     checked = state.plotPeriod === PlotPeriod.Semester
            ),
            <.label(PlotPeriod.Semester.toString,
                    ^.cursor.pointer,
                    ^.onClick --> $.setStateL(State.plotPeriod)(PlotPeriod.Semester)
            )
          ),
          <.div(ExploreStyles.PlotToggleCheckbox)(
            <.label(TimeDisplay.UTC.toString,
                    ^.cursor.pointer,
                    ^.onClick --> $.setStateL(State.timeDisplay)(TimeDisplay.UTC)
            ),
            Checkbox(slider = true,
                     clazz = ExploreStyles.PlotToggle,
                     onClick = toggleTimeDisplay,
                     checked = state.timeDisplay === TimeDisplay.Site
            ),
            <.label(TimeDisplay.Site.toString,
                    ^.cursor.pointer,
                    ^.onClick --> $.setStateL(State.timeDisplay)(TimeDisplay.Site)
            )
          )
        )
      )
    }

  }

  val component =
    ScalaComponent
      .builder[Props]
      .initialState(
        State(Site.GS,
              ZonedDateTime.now(Site.GS.timezone).toLocalDate.plusDays(1),
              PlotPeriod.Night,
              TimeDisplay.UTC
        )
      )
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .build
}
