// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Eq
import cats.syntax.all._
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.Site
import lucuma.core.math.Coordinates
import lucuma.ui.reusability._
import monocle.macros.Lenses
import react.common.ReactProps
import react.datepicker._
import react.semanticui.modules.checkbox.Checkbox

import java.time.LocalDate
import java.time.ZoneId
import java.time.ZoneOffset
import java.time.ZonedDateTime

final case class SkyPlotSection(
  coords: Coordinates
) extends ReactProps[SkyPlotSection](SkyPlotSection.component)

object SkyPlotSection {
  type Props = SkyPlotSection

  protected sealed trait TimeDisplay
  protected object TimeDisplay {
    final case object UTC  extends TimeDisplay
    final case object Site extends TimeDisplay

    implicit val TimeDisplayEq: Eq[TimeDisplay]             = Eq.fromUniversalEquals
    implicit val TimeDisplayReuse: Reusability[TimeDisplay] = Reusability.byEq
  }

  @Lenses
  final case class State(site: Site, date: LocalDate, timeDisplay: TimeDisplay) {
    def zoneId: ZoneId =
      timeDisplay match {
        case TimeDisplay.UTC  => ZoneOffset.UTC
        case TimeDisplay.Site => site.timezone
      }
  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive
  implicit val stateReuse: Reusability[State] = Reusability.derive

  class Backend($ : BackendScope[Props, State]) {
    val toggleSite: Callback =
      $.modStateL(State.site) {
        case Site.GS => Site.GN
        case Site.GN => Site.GS
      }

    val toggleTimeDisplay: Callback =
      $.modStateL(State.timeDisplay) {
        case TimeDisplay.UTC  => TimeDisplay.Site
        case TimeDisplay.Site => TimeDisplay.UTC
      }

    def render(props: Props, state: State) =
      <.div(ExploreStyles.SkyPlotSection)(
        <.div(ExploreStyles.SkyPlot)(
          SkyPlot(state.site, props.coords, state.date, state.zoneId, 350)
        ),
        <.div(ExploreStyles.SkyPlotControls)(
          <.div(
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
          <.div(
            Datepicker(onChange =
              (newValue, _) => $.setStateL(State.date)(newValue.toLocalDateOpt.get)
            )
              .selected(state.date.toJsDate)
              .dateFormat("yyyy-MM-dd")
          ),
          <.div(
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

  val component =
    ScalaComponent
      .builder[Props]
      .initialState(
        State(Site.GS, ZonedDateTime.now(Site.GS.timezone).toLocalDate.plusDays(1), TimeDisplay.UTC)
      )
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .build
}
