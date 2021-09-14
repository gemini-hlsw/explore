// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Eq
import cats.syntax.all._
import eu.timepit.refined.auto._
import explore._
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.targeteditor.SkyPlotNight.TimeDisplay
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
import react.semanticui.collections.form.Form
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.ButtonGroup
import react.semanticui.elements.segment.Segment

import java.time.LocalDate
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

  final case class State(
    site:        Site,
    date:        LocalDate,
    plotPeriod:  PlotPeriod,
    timeDisplay: TimeDisplay
  )

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

    def render(props: Props, state: State) = {
      implicit val ctx = props.ctx

      Segment(ExploreStyles.SkyPlotSection)(
        HelpIcon("target/main/elevation-plot.md", ExploreStyles.HelpIconFloating),
        <.div(ExploreStyles.SkyPlot) {
          state.plotPeriod match {
            case PlotPeriod.Night    =>
              SkyPlotNight(state.site, props.coords, state.date, state.timeDisplay, 350)
            case PlotPeriod.Semester =>
              val site     = state.site
              val coords   = props.coords
              val semester = Semester.fromLocalDate(state.date)
              SkyPlotSemester(site, coords, semester, 350).withKey(
                s"$site-$coords-$semester"
              )
          }
        },
        Form(clazz = ExploreStyles.SkyPlotControls)(
          ButtonGroup(compact = true)(
            Button(
              active = state.site === Site.GN,
              onClick = $.setStateL(State.site)(Site.GN)
            )("GN"),
            Button(
              active = state.site === Site.GS,
              onClick = $.setStateL(State.site)(Site.GS)
            )("GS")
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
          ButtonGroup(compact = true)(
            Button(
              active = state.plotPeriod === PlotPeriod.Night,
              onClick = $.setStateL(State.plotPeriod)(PlotPeriod.Night)
            )("Night"),
            Button(
              active = state.plotPeriod === PlotPeriod.Semester,
              onClick = $.setStateL(State.plotPeriod)(PlotPeriod.Semester)
            )("Semester")
          ),
          ButtonGroup(compact = true)(
            Button(
              active = state.timeDisplay === TimeDisplay.UTC,
              onClick = $.setStateL(State.timeDisplay)(TimeDisplay.UTC)
            )("UT"),
            Button(
              active = state.timeDisplay === TimeDisplay.Sidereal,
              onClick = $.setStateL(State.timeDisplay)(TimeDisplay.Sidereal)
            )("Sidereal"),
            Button(
              active = state.timeDisplay === TimeDisplay.Site,
              onClick = $.setStateL(State.timeDisplay)(TimeDisplay.Site)
            )("Site")
          )((^.visibility.hidden.when(state.plotPeriod === PlotPeriod.Semester)))
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
              TimeDisplay.Site
        )
      )
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .build
}
