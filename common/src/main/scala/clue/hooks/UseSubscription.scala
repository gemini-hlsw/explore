// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package clue.hooks

import crystal.Pot
import crystal.react.hooks._
import japgolly.scalajs.react._
import japgolly.scalajs.react.hooks.CustomHook

// unprotect in crystal so we can reuse it here
protected[hooks] final case class WithDeps[D, A](deps: D, fromDeps: D => A)

object UseSubscription {
  def hook[D: Reusability, S, A] =
    CustomHook[WithDeps[D, GraphQLSubscription[S, A]]]
      .useStreamResourceBy(props => props.deps)(props => deps => props.fromDeps(deps).subscribe)
      .buildReturning((_, value) => value)

  object HooksApiExt {
    sealed class Primary[Ctx, Step <: HooksApi.AbstractStep](api: HooksApi.Primary[Ctx, Step]) {
      final def useSubscription[D: Reusability, S, A](
        deps:     => D
      )(resource: D => GraphQLSubscription[S, A])(implicit
        step:     Step
      ): step.Next[Pot[A]] =
        useSubscriptionBy(_ => deps)(_ => resource)

      final def useSubscriptionOnMount[S, A](resource: GraphQLSubscription[S, A])(implicit
        step:                                          Step
      ): step.Next[Pot[A]] =
        useSubscriptionOnMountBy(_ => resource)

      final def useSubscriptionBy[D: Reusability, S, A](
        deps:     Ctx => D
      )(resource: Ctx => D => GraphQLSubscription[S, A])(implicit
        step:     Step
      ): step.Next[Pot[A]] =
        api.customBy { ctx =>
          val hookInstance = hook[D, S, A]
          hookInstance(WithDeps(deps(ctx), resource(ctx)))
        }

      final def useSubscriptionOnMountBy[S, A](resource: Ctx => GraphQLSubscription[S, A])(implicit
        step:                                            Step
      ): step.Next[Pot[A]] = // () has Reusability = always.
        useSubscriptionBy(_ => ())(ctx => _ => resource(ctx))
    }

    final class Secondary[Ctx, CtxFn[_], Step <: HooksApi.SubsequentStep[Ctx, CtxFn]](
      api: HooksApi.Secondary[Ctx, CtxFn, Step]
    ) extends Primary[Ctx, Step](api) {

      /**
       * Open a `Resource[Async, A]` on mount or when dependencies change, and close it on unmount
       * or when dependencies change. Provided as a `Pot[A]`. Will rerender when the `Pot` state
       * changes.
       */
      def useSubscriptionBy[D: Reusability, S, A](
        deps:     CtxFn[D]
      )(resource: CtxFn[D => GraphQLSubscription[S, A]])(implicit
        step:     Step
      ): step.Next[Pot[A]] =
        useSubscriptionBy(step.squash(deps)(_))(step.squash(resource)(_))

      /**
       * Open a `Resource[Async, A]` on mount and close it on unmount. Provided as a `Pot[A]`. Will
       * rerender when the `Pot` state changes.
       */
      final def useSubscriptionOnMountBy[S, A](resource: CtxFn[GraphQLSubscription[S, A]])(implicit
        step:                                            Step
      ): step.Next[Pot[A]] =
        useSubscriptionOnMountBy(step.squash(resource)(_))
    }
  }

  trait HooksApiExt {
    import HooksApiExt._

    implicit def hooksExtSubscription1[Ctx, Step <: HooksApi.AbstractStep](
      api: HooksApi.Primary[Ctx, Step]
    ): Primary[Ctx, Step] =
      new Primary(api)

    implicit def hooksExtSubscription2[Ctx, CtxFn[_], Step <: HooksApi.SubsequentStep[Ctx, CtxFn]](
      api: HooksApi.Secondary[Ctx, CtxFn, Step]
    ): Secondary[Ctx, CtxFn, Step] =
      new Secondary(api)
  }

  object implicits extends HooksApiExt
}
