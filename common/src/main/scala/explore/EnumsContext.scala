// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import clue.StreamingClient
import crystal.react.hooks.*
import explore.Enums
import explore.model.AppContext
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.Context
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.schemas.ObservationDB
import lucuma.ui.syntax.pot.*

object EnumsContext {
  val ctx: Context[Enums] = React.createContext(null) // No default value

  val Provider =
    ScalaFnComponent
      .withHooks[Unit]
      .withPropsChildren
      .useContext(AppContext.ctx)
      .useEffectResultOnMountBy { (_, _, ctx) =>
        import ctx.given

        Enums.load()
      }
      .render((_, children, _, enumsPot) =>
        enumsPot.renderPot(EnumsContext.ctx.provide(_)(children))
      )
}
