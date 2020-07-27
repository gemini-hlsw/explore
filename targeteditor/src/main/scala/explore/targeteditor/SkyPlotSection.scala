// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import java.time.LocalDate
import java.time.ZoneId
import java.time.ZoneOffset

import cats.Eq
import cats.implicits._
import explore.components.ui.GPPStyles
import explore.model.reusability._
import gem.enum.Site
import gpp.ui.reusability._
import gsp.math.Coordinates
import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import react.common.ReactProps
import react.semanticui.modules.checkbox.Checkbox

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
  final case class State(site: Site, timeDisplay: TimeDisplay) {
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
      <.div(GPPStyles.SkyPlotSection)(
        <.div(GPPStyles.SkyPlot)(
          SkyPlot(state.site, props.coords, LocalDate.of(2020, 7, 22), state.zoneId, 350)
        ),
        <.div(GPPStyles.SkyPlotControls)(
          <.div(
            <.label(Site.GN.toString,
                    ^.cursor.pointer,
                    ^.onClick --> $.setStateL(State.site)(Site.GN)
            ),
            Checkbox(slider = true,
                     clazz = GPPStyles.PlotToggle,
                     onClick = toggleSite,
                     checked = state.site === Site.GS
            ),
            <.label(Site.GS.toString,
                    ^.cursor.pointer,
                    ^.onClick --> $.setStateL(State.site)(Site.GS)
            )
          ),
          <.div(
            <.label(TimeDisplay.UTC.toString,
                    ^.cursor.pointer,
                    ^.onClick --> $.setStateL(State.timeDisplay)(TimeDisplay.UTC)
            ),
            Checkbox(slider = true,
                     clazz = GPPStyles.PlotToggle,
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
      .initialState(State(Site.GS, TimeDisplay.UTC))
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .build
}
