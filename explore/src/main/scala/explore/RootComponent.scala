// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import crystal.react.hooks.*
import crystal.syntax.*
import explore.model.AppContext
import explore.model.Page
import explore.model.ProgramSummaries
import explore.model.RootModel
import explore.model.RootModelViews
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.RouterWithProps
import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.*
import lucuma.ui.syntax.all.*

import scala.concurrent.duration.*
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

case class RootComponent(
  ctx:          AppContext[IO],
  router:       RouterWithProps[Page, RootModelViews],
  initialModel: RootModel
) extends ReactFnProps(RootComponent)

object RootComponent
    extends ReactFnComponent[RootComponent](props =>
      for
        rootModel                       <- useStateView(props.initialModel)
        attr: Option[ResourceAttributes] = rootModel.get.vault.map(ResourceAttributes.fromUserVault)
        programSummariesPot             <- useThrottlingStateView(pending[ProgramSummaries], 5.seconds)
      yield AppContext.ctx.provide(props.ctx):
        React.Fragment(
          props.ctx.tracing.map: c =>
            Observability(HoneycombOptions(c.key, c.serviceName, attr.orUndefined)),
          HelpContext.Provider:
            programSummariesPot.renderPot: programSummaries =>
              props.router(RootModelViews(rootModel, programSummaries))
        )
    )
