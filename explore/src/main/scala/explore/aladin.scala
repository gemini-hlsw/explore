// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.aladin

import cats.syntax.all.*
import crystal.react.ViewOpt
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.AladinFullScreen
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.aladin.Aladin
import react.common.ReactFnProps
import react.common.style.Css
import react.primereact.Button

case class AladinZoomControl(
  aladinRef: Ref.ToScalaComponent[Aladin, Aladin.State, Aladin.Backend],
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
          onClick = p.aladinRef.get.asCBO.flatMapCB(_.backend.increaseZoom).toCallback
        ).small,
        Button(
          clazz = ExploreStyles.ButtonOnAladin,
          icon = Icons.ThinMinus,
          onClick = p.aladinRef.get.asCBO.flatMapCB(_.backend.decreaseZoom).toCallback
        ).small
      ),
    )
}

case class AladinFullScreenControl(
  fullScreen: ViewOpt[AladinFullScreen]
) extends ReactFnProps(AladinFullScreenControl.component)

object AladinFullScreenControl {
  private type Props = AladinFullScreenControl

  private val component =
    ScalaFnComponent[Props](p =>
      Button(onClick = p.fullScreen.mod(_.flip))
        .withMods(
          ExploreStyles.ButtonOnAladin |+| ExploreStyles.AladinFullScreenButton,
          Icons.ExpandDiagonal.unless(p.fullScreen.get.map(_.value).getOrElse(true)),
          Icons.ContractDiagonal.when(p.fullScreen.get.map(_.value).getOrElse(false))
        )
        .small,
    )
}
