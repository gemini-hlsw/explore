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

object hooks extends HooksApiExt
