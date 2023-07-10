// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnPropsWithChildren
import react.common.style.Css

case class MoonPhase(moonPhase: Double, clazz: Css = Css.Empty)
    extends ReactFnPropsWithChildren[MoonPhase](MoonPhase.component)

object MoonPhase {
  private type Props = MoonPhase

  private val component = ScalaFnComponent.withChildren[Props]((p, c) =>
    <.div(ExploreStyles.MoonPhase)(
      // Adapted from https://dev.to/thormeier/use-your-i-moon-gination-lets-build-a-moon-phase-visualizer-with-css-and-js-aih
      <.div(ExploreStyles.MoonSphere)(
        <.div(ExploreStyles.MoonDark).when(p.moonPhase < 0.5),
        <.div(ExploreStyles.MoonLight).when(p.moonPhase > 0.5),
        <.div(ExploreStyles.MoonDark).when(p.moonPhase > 0.5),
        <.div(ExploreStyles.MoonLight).when(p.moonPhase < 0.5),
        <.div(ExploreStyles.MoonDivider,
              ^.transform := s"rotate3d(0, 1, 0, ${360 * p.moonPhase}deg"
        )
      ),
      c
    )
  )
}
