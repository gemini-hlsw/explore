// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package react.hotkeys

import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.TopNode

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

object HooksApiExt {

  private def hook: CustomHook[UseHotkeysProps, Ref] =
    CustomHook[UseHotkeysProps]
      .buildReturning { pos =>
        use.useHotkeys(pos)
      }

  sealed class Primary[Ctx, Step <: HooksApi.AbstractStep](api: HooksApi.Primary[Ctx, Step]) {

    final def useHotkeys(pos: UseHotkeysProps)(using
      step:                   Step
    ): step.Next[Ref] =
      useHotkeysBy(_ => pos)

    final def useHotkeysBy(pos: Ctx => UseHotkeysProps)(using
      step:                     Step
    ): step.Next[Ref] =
      api.customBy(ctx => hook(pos(ctx)))

  }

  final class Secondary[Ctx, CtxFn[_], Step <: HooksApi.SubsequentStep[Ctx, CtxFn]](
    api: HooksApi.Secondary[Ctx, CtxFn, Step]
  ) extends Primary[Ctx, Step](api) {

    def useHotkeysBy(pos: CtxFn[UseHotkeysProps])(using
      step:               Step
    ): step.Next[Ref] =
      useHotkeysBy(step.squash(pos)(_))

  }
}

object HooksApiExtGlobal {
  private val hook: CustomHook[UseHotkeysProps, Unit] =
    CustomHook[UseHotkeysProps]
      .buildReturning { pos =>
        use.useHotkeys(pos)
      }
  // def hook[D: Reusability] = CustomHook[WithDeps[D, ]]
  //   .useState(Pot.pending[A])
  //   .useEffectWithDepsBy((props, _) => props.deps)((_, state) => _ => state.setState(Pot.pending))
  //   .useEffectWithDepsBy((props, _) => props.deps)((props, state) =>
  //     deps =>
  //       (for {
  //         a <- props.fromDeps(deps)
  //         _ <- state.setStateAsync(a.ready)
  //       } yield ()).handleErrorWith(t => state.setStateAsync(Pot.error(t)))
  //   )
  //   .buildReturning((_, state) => state.value)

  sealed class Primary[Ctx, Step <: HooksApi.AbstractStep](api: HooksApi.Primary[Ctx, Step]) {

    final def useGlobalHotkeys(pos: UseHotkeysProps)(using
      step:                         Step
    ): step.Self =
      useGlobalHotkeysBy(_ => pos)

    final def useGlobalHotkeysBy(pos: Ctx => UseHotkeysProps)(using
      step:                           Step
    ): step.Self =
      api.customBy[Unit](ctx => hook(pos(ctx)))

    // final def useGlobalHotkeysWithDepsBy[D](deps: CtxFn[D])(effect: CtxFn[D => A]): step.Self =
    // final def useGlobalHotkeysWithDepsBy[D, A](deps:                      Ctx => D)(
    //   effect:                                                             Ctx => D => A
    // )(
    //   implicit /*a:                                UseEffectArg[A], */ r: Reusability[D],
    //   step:                                                               Step
    // ): step.Self =
    //   useGlobalHotkeysWithDepsBy(step.squash(deps)(_))(step.squash(effect)(_))
  }

  final class Secondary[Ctx, CtxFn[_], Step <: HooksApi.SubsequentStep[Ctx, CtxFn]](
    api: HooksApi.Secondary[Ctx, CtxFn, Step]
  ) extends Primary[Ctx, Step](api) {

    def useGlobalHotkeysBy(pos: CtxFn[UseHotkeysProps])(using
      step:                     Step
    ): step.Self =
      useGlobalHotkeysBy(step.squash(pos)(_))

  }
}

trait HooksApiExt {
  import HooksApiExt._
  import scala.language.implicitConversions

  implicit def hooksExtHotkeys1[Ctx, Step <: HooksApi.AbstractStep](
    api: HooksApi.Primary[Ctx, Step]
  ): Primary[Ctx, Step] =
    new Primary(api)

  implicit def hooksExtHotkeys2[Ctx, CtxFn[_], Step <: HooksApi.SubsequentStep[Ctx, CtxFn]](
    api: HooksApi.Secondary[Ctx, CtxFn, Step]
  ): Secondary[Ctx, CtxFn, Step] =
    new Secondary(api)
}

trait HooksApiExtGlobal {
  import HooksApiExtGlobal.*
  import scala.language.implicitConversions

  implicit def hooksExtGlobalHotkeys1[Ctx, Step <: HooksApi.AbstractStep](
    api: HooksApi.Primary[Ctx, Step]
  ): Primary[Ctx, Step] =
    new Primary(api)

  implicit def hooksExtGlobalHotkeys2[Ctx, CtxFn[_], Step <: HooksApi.SubsequentStep[Ctx, CtxFn]](
    api: HooksApi.Secondary[Ctx, CtxFn, Step]
  ): Secondary[Ctx, CtxFn, Step] =
    new Secondary(api)
}

object hooks extends HooksApiExt with HooksApiExtGlobal
