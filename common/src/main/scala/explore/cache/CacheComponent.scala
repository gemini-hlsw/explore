// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.Context
import japgolly.scalajs.react.vdom.html_<^.*
import crystal.react.hooks.*
import lucuma.ui.syntax.pot.*
import cats.effect.Resource
import japgolly.scalajs.react.util.DefaultEffects.{Async => DefaultA}
import fs2.concurrent.SignallingRef
import cats.effect.kernel.Deferred
import cats.syntax.all.given
import cats.effect.syntax.all.given
import crystal.react.View

trait CacheComponent[Props: Reusability, A]:
  private type F[T] = DefaultA[T]

  protected val initial: Props => F[A]

  protected val updateStream: Props => Resource[F, fs2.Stream[F, A => A]]

  val view: Context[View[A]] = React.createContext(null) // No default value

  val Provider =
    ScalaFnComponent
      .withHooks[Props]
      .withPropsChildren
      .useEffectResultWithDepsBy(_.props)(_ =>
        props =>
          for
            latch        <- Deferred[F, SignallingRef[F, A]]
            _            <-                       // Start the update fiber. Will only update once cache is initialized (via latch)
              updateStream(props)
                .evalTap(
                  _.evalTap(mod => latch.get.flatMap(_.update(mod))).compile.drain
                )
                .useForever
                .start
            initialValue <- initial(props)
            cache        <- SignallingRef[F].of(initialValue)
            _            <- latch.complete(cache) // Allow stream updates to proceed.
          yield cache
      )
      .useStreamViewBy((props, _, cache) => (props, cache.isReady))((_, _, cache) =>
        _ => cache.toOption.map(_.discrete).orEmpty
      )
      .render((_, children, _, cacheView) => cacheView.renderPotOption(view.provide(_)(children)))
