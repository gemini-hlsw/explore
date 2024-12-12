// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.monadicHooks

import cats.Monad
import scala.annotation.tailrec
import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.hooks.CustomHook
// import japgolly.scalajs.react.hooks.HookComponentBuilder
import japgolly.scalajs.react.component.ScalaFn
import japgolly.scalajs.react.CtorType
import sourcecode.FullName

// Hook results are suspended to avoid them being executed anytime
opaque type HookResult[A] = () => A

object HookResult:
  def apply[A](a: => A): HookResult[A] = () => a

// Not sure about this, it can still be executed anywhere.
// Maybe it's better to have a ScalaFnComponent constructor that accepts this and only it can execute.
// given Conversion[HookResult[VdomNode], VdomNode] with
//   inline def apply(hr: HookResult[VdomNode]): VdomNode = hr()

extension (scalafn: ScalaFn.type)
  def withFnHooks[P](render: P => HookResult[VdomNode])(using
    name: FullName
  ): ScalaFn.Component[P, CtorType.Props] =
    ScalaFn: props =>
      render(props)()

// extension [P](builder: HookComponentBuilder.ComponentP.First[P])
//   // def monadic(body: P => HookResult[VdomNode]): ScalaFn.Component[P, CtorType.Props] =
//   def apply(body: P => HookResult[VdomNode]): ScalaFn.Component[P, CtorType.Props] =
//     builder.render(p => body(p)())

// TODO A way to create CustomHooks monadically

extension [O](hook: CustomHook[Unit, O]) def lift: HookResult[O] = HookResult(hook.unsafeInit(()))

extension [I, O](hook: CustomHook[I, O])
  def lift: I => HookResult[O] = (i: I) => HookResult(hook.unsafeInit(i))

// TODO TEST
given Monad[HookResult] with

  def pure[A](a: A): HookResult[A] =
    () => a

  def flatMap[A, B](fa: HookResult[A])(f: A => HookResult[B]): HookResult[B] =
    () => f(fa())()

  @tailrec
  override def tailRecM[A, B](init: A)(f: A => HookResult[Either[A, B]]): HookResult[B] =
    f(init)() match
      case Left(a)  => tailRecM(a)(f)
      case Right(b) => pure(b)
