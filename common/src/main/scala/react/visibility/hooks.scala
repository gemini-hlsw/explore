// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package react.visibility

import japgolly.scalajs.react.*
import cats.syntax.all.*
import org.scalajs.dom

import scalajs.js
import scalajs.js.JSConverters.*
import org.scalajs.dom.IntersectionObserver
import org.scalajs.dom.IntersectionObserverOptions
import org.scalajs.dom.IntersectionObserverEntry

type ElementRef = Ref.ToVdom[dom.html.Element]

@js.native
trait UseVisibilityProps extends js.Object {
  val ref: ElementRef
}

val hook = CustomHook[ElementRef]
  // Visibility
  .useState(false)
  // IntersectionObserver
  .useRefBy { (ref, isIntersectingSt) =>
    // println("Create Observer")
    org.scalajs.dom.window.console.log(ref.raw)
    new IntersectionObserver(
      (entries: js.Array[IntersectionObserverEntry], _: IntersectionObserver) => {
        // Option(entries).foreach(_ => org.scalajs.dom.window.console.log(entries))
        val isIntersecting = entries.exists(_.isIntersecting)
        // println(s"IntersectionObserver $isIntersecting")
        isIntersectingSt.setState(isIntersecting).runNow()
      }
      // IntersectionObserverOptions(root =
      //   Option(
      //     org.scalajs.dom.document.getElementsByClassName("obs-tree-wrapper").item(0)
      //   ).orUndefined
      // ) // , rootMargin = "0px", threshold = 0.0)
    )
    // println(obs)
  }
  .useEffectBy { // WithDepsBy((ref, isIntersectingSt, observer) => ref) {
    (ref, isIntersectingSt, o) => 
      val observer = o.raw.current
      val node     = Option(ref.raw.current)
      // Observe current
      // println(ref.raw)
      // println(node)
      node.foreach { cur =>
        println("Observe"); observer.observe(cur.domCast[dom.html.Element])
      }
      // Cleanup
      CallbackTo[Callback] {
        Callback {
          node.foreach(cur => observer.unobserve(cur.domCast[dom.html.Element]))
        }
      }
  }
  .buildReturning { (_, visible, _) =>
    visible.value
  }

object HooksApiExt {
  sealed class Primary[Ctx, Step <: HooksApi.AbstractStep](api: HooksApi.Primary[Ctx, Step]) {

    final def useVisibility(ref: ElementRef)(implicit step: Step): step.Next[Boolean] =
      useVisibilityHookBy(_ => ref)

    final def useVisibilityHookBy(ref: Ctx => ElementRef)(implicit step: Step): step.Next[Boolean] =
      api.customBy(ctx => hook(ref(ctx)))
  }

  final class Secondary[Ctx, CtxFn[_], Step <: HooksApi.SubsequentStep[Ctx, CtxFn]](
    api: HooksApi.Secondary[Ctx, CtxFn, Step]
  ) extends Primary[Ctx, Step](api) {

    def useVisibilityHookBy(name: CtxFn[ElementRef])(implicit step: Step): step.Next[Boolean] =
      useVisibilityHookBy(step.squash(name)(_))
  }
}

trait HooksApiExt {
  import HooksApiExt._

  implicit def hooksExtMyCustomHook1[Ctx, Step <: HooksApi.AbstractStep](
    api: HooksApi.Primary[Ctx, Step]
  ): Primary[Ctx, Step] =
    new Primary(api)

  implicit def hooksExtMyCustomHook2[Ctx, CtxFn[_], Step <: HooksApi.SubsequentStep[Ctx, CtxFn]](
    api: HooksApi.Secondary[Ctx, CtxFn, Step]
  ): Secondary[Ctx, CtxFn, Step] =
    new Secondary(api)
}

object hooks extends HooksApiExt
