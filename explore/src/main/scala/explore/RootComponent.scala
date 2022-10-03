// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import crystal.react.View
import crystal.react.hooks.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Page
import explore.model.RootModel
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.RouterWithProps
import japgolly.scalajs.react.vdom.html_<^.*
import react.common.*
import react.toastify.*

case class RootComponent(
  ctx:          AppContext[IO],
  router:       RouterWithProps[Page, View[RootModel]],
  initialModel: RootModel
) extends ReactFnProps(RootComponent.component)

object RootComponent:
  private type Props = RootComponent

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateViewBy(_.initialModel)
      .render((props, rootModel) =>
        AppContext.ctx.provide(props.ctx)(
          HelpContext.Provider(
            <.div(
              props.router(rootModel),
              ToastContainer(
                position = Position.BottomRight,
                theme = react.toastify.Theme.Dark,
                clazz = ExploreStyles.ExploreToast
              )
            )
          )
        )
      )
