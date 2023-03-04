// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package react.hotkeys

import japgolly.scalajs.react.*

import scalajs.js

private def useHotkeysHook[D: Reusability](
  deps: => D
)(props: D => UseHotkeysProps): CustomHook[Unit, Ref] =
  CustomHook
    .reusableDeps[D]
    .apply(() => deps)
    .map { case (d, rev) =>
      val p = props(d)
      Ref.fromJs(useHotkeys(p.keys, p.callback, p.options, js.Array(rev)))
    }

object HooksApiExt {

  sealed class Primary[Ctx, Step <: HooksApi.AbstractStep](api: HooksApi.Primary[Ctx, Step]) {
    final def useHotkeysWithDeps[D: Reusability](
      deps: => D
    )(props: D => UseHotkeysProps)(using
      step: Step
    ): step.Next[Ref] =
      useHotkeysWithDepsBy(_ => deps)(_ => props)

    final def useHotkeys(props: UseHotkeysProps)(using
      step: Step
    ): step.Next[Ref] =
      useHotkeysBy(_ => props)

    final def useHotkeysBy(props: Ctx => UseHotkeysProps)(using
      step: Step
    ): step.Next[Ref] =
      useHotkeysWithDepsBy(_ => Reusable.never(()))(ctx => _ => props(ctx))

    final def useHotkeysWithDepsBy[D: Reusability](
      deps: Ctx => D
    )(props: Ctx => D => UseHotkeysProps)(using step: Step): step.Next[Ref] =
      api.customBy { ctx =>
        val hookInstance = useHotkeysHook[D]
        hookInstance(deps(ctx))(props(ctx))
      }
  }

  final class Secondary[Ctx, CtxFn[_], Step <: HooksApi.SubsequentStep[Ctx, CtxFn]](
    api: HooksApi.Secondary[Ctx, CtxFn, Step]
  ) extends Primary[Ctx, Step](api) {
    def useHotkeysWithDepsBy[D: Reusability](
      deps: CtxFn[D]
    )(props: CtxFn[D => UseHotkeysProps])(using
      step: Step
    ): step.Next[Ref] =
      useHotkeysWithDepsBy(step.squash(deps)(_))(step.squash(props)(_))

    def useHotkeysBy(pos: CtxFn[UseHotkeysProps])(using
      step: Step
    ): step.Next[Ref] =
      useHotkeysBy(step.squash(pos)(_))
  }
}

object HooksApiExtGlobal {
  sealed class Primary[Ctx, Step <: HooksApi.AbstractStep](api: HooksApi.Primary[Ctx, Step]) {

    final def useGlobalHotkeysWithDeps[D: Reusability](
      deps: => D
    )(props: D => UseHotkeysProps)(using
      step: Step
    ): step.Self =
      useGlobalHotkeysWithDepsBy(_ => deps)(_ => props)

    final def useGlobalHotkeys(props: UseHotkeysProps)(using
      step: Step
    ): step.Self =
      useGlobalHotkeysBy(_ => props)

    final def useGlobalHotkeysBy(props: Ctx => UseHotkeysProps)(using
      step: Step
    ): step.Self =
      useGlobalHotkeysWithDepsBy(_ => Reusable.never(()))(ctx => _ => props(ctx))

    final def useGlobalHotkeysWithDepsBy[D: Reusability](
      deps: Ctx => D
    )(props: Ctx => D => UseHotkeysProps)(using step: Step): step.Self =
      api.customBy { ctx =>
        val hookInstance = useHotkeysHook[D]
        hookInstance(deps(ctx))(props(ctx)).map(_ => ())
      }
  }

  final class Secondary[Ctx, CtxFn[_], Step <: HooksApi.SubsequentStep[Ctx, CtxFn]](
    api: HooksApi.Secondary[Ctx, CtxFn, Step]
  ) extends Primary[Ctx, Step](api) {

    def useGlobalHotkeysWithDepsBy[D: Reusability](
      deps: CtxFn[D]
    )(props: CtxFn[D => UseHotkeysProps])(implicit
      step: Step
    ): step.Self =
      useGlobalHotkeysWithDepsBy(step.squash(deps)(_))(step.squash(props)(_))

    def useGlobalHotkeysBy(pos: CtxFn[UseHotkeysProps])(using
      step: Step
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
