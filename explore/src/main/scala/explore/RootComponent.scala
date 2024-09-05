// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import cats.syntax.option.*
import crystal.react.hooks.*
import explore.model.AppContext
import explore.model.Page
import explore.model.ProgramSummaries
import explore.model.RootModel
import explore.model.RootModelViews
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.RouterWithProps
import lucuma.react.common.*
import lucuma.ui.syntax.all.*

import scala.concurrent.duration.*

case class RootComponent(
  ctx:          AppContext[IO],
  router:       RouterWithProps[Page, RootModelViews],
  initialModel: RootModel
) extends ReactFnProps(RootComponent.component)

object RootComponent:
  private type Props = RootComponent

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateViewBy(_.initialModel)
      .useThrottlingStateView(none[ProgramSummaries], 2.seconds)
      .render: (props, rootModel, programSummariesPot) =>
        AppContext.ctx.provide(props.ctx):
          HelpContext.Provider:
            programSummariesPot.renderPot: programSummaries =>
              props.router(RootModelViews(rootModel, programSummaries))
