// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package react.floatingui

import japgolly.scalajs.react._

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.|

@js.native
trait ElementProps extends js.Object

type ElementPropsList = List[ElementProps]

object HooksApiExt {
  private val floatingHook =
    CustomHook[UseFloatingProps]
      .buildReturning { pos =>
        use.useFloating(pos)
      }

  private val interactionsHook =
    CustomHook[ElementPropsList]
      .buildReturning { ctx =>
        use.useInteractions(ctx.toJSArray)
      }

  sealed class Primary[Ctx, Step <: HooksApi.AbstractStep](api: HooksApi.Primary[Ctx, Step]) {

    final def useFloating(pos: UseFloatingProps = UseFloatingProps())(implicit
      step:                    Step
    ): step.Next[UseFloatingReturn] =
      useFloatingBy(_ => pos)

    final def useFloatingBy(pos: Ctx => UseFloatingProps)(implicit
      step:                      Step
    ): step.Next[UseFloatingReturn] =
      api.customBy(ctx => floatingHook(pos(ctx)))

    final def useInteractions(pos: ElementPropsList)(implicit
      step:                        Step
    ): step.Next[UseFloatingReturn] =
      useInteractionsBy(_ => pos)

    final def useInteractionsBy(pos: Ctx => ElementPropsList)(implicit
      step:                          Step
    ): step.Next[UseFloatingReturn] =
      api.customBy(ctx => interactionsHook(pos(ctx)))
  }

  final class Secondary[Ctx, CtxFn[_], Step <: HooksApi.SubsequentStep[Ctx, CtxFn]](
    api: HooksApi.Secondary[Ctx, CtxFn, Step]
  ) extends Primary[Ctx, Step](api) {

    def useFloatingBy(pos: CtxFn[UseFloatingProps])(implicit
      step:                Step
    ): step.Next[UseFloatingReturn] =
      useFloatingBy(step.squash(pos)(_))

    def useInteractionsBy(pos: CtxFn[ElementPropsList])(implicit
      step:                    Step
    ): step.Next[UseFloatingReturn] =
      useInteractionsBy(step.squash(pos)(_))

  }
}

trait HooksApiExt {
  import HooksApiExt._
  import scala.language.implicitConversions

  implicit def hooksExtFloating1[Ctx, Step <: HooksApi.AbstractStep](
    api: HooksApi.Primary[Ctx, Step]
  ): Primary[Ctx, Step] =
    new Primary(api)

  implicit def hooksExtFloating2[Ctx, CtxFn[_], Step <: HooksApi.SubsequentStep[Ctx, CtxFn]](
    api: HooksApi.Secondary[Ctx, CtxFn, Step]
  ): Secondary[Ctx, CtxFn, Step] =
    new Secondary(api)
}

object hooks extends HooksApiExt
