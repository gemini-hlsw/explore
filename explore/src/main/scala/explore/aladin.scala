// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.aladin

import cats.syntax.all.*
import crystal.react.View
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.AladinFullScreen
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnProps
import lucuma.react.common.style.Css
import lucuma.react.primereact.Button
import lucuma.ui.aladin.*
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given

case class AladinZoomControl(
  aladinRef: Aladin,
  clazz:     Css = Css.Empty
) extends ReactFnProps(AladinZoomControl.component)

object AladinZoomControl {
  private type Props = AladinZoomControl

  private val component =
    ScalaFnComponent[Props](p =>
      <.div(
        ExploreStyles.AladinZoomControl |+| p.clazz,
        Button(
          clazz = ExploreStyles.ButtonOnAladin,
          icon = Icons.ThinPlus,
          onClick = p.aladinRef.increaseZoomCB
        ).small,
        Button(
          clazz = ExploreStyles.ButtonOnAladin,
          icon = Icons.ThinMinus,
          onClick = p.aladinRef.decreaseZoomCB
        ).small
      )
    )
}

case class AladinFullScreenControl(
  fullScreen: View[AladinFullScreen]
) extends ReactFnProps(AladinFullScreenControl.component)

object AladinFullScreenControl {
  private type Props = AladinFullScreenControl

  private val component =
    ScalaFnComponent[Props](p =>
      Button(onClick = p.fullScreen.mod(_.flip))
        .withMods(
          ExploreStyles.ButtonOnAladin |+| ExploreStyles.AladinFullScreenButton,
          Icons.ExpandDiagonal.unless(p.fullScreen.get.value),
          Icons.ContractDiagonal.when(p.fullScreen.get.value)
        )
        .small
    )
}
