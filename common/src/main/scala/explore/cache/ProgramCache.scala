// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.effect.IO
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.Context
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import crystal.react.hooks.*
import lucuma.ui.reusability.given
import explore.model.AppContext
import lucuma.ui.syntax.pot.*

object ProgramCache:
  val ctx: Context[ModelCaches[IO]] = React.createContext(null) // No default value

  val Provider =
    ScalaFnComponent
      .withHooks[Program.Id]
      .withPropsChildren
      .useContext(AppContext.ctx)
      // FIXME This will be useResource later on
      .useEffectResultWithDepsBy((programId, _, _) => programId)((_, _, appCtx) =>
        programId =>
          import appCtx.given

          ModelCaches.forProgram(programId)
      )
      .render((_, children, _, caches) => caches.renderPot(ctx.provide(_)(children)))
