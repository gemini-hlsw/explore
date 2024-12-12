// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.monadicHooks

import cats.Monad
import japgolly.scalajs.react.CtorType
import japgolly.scalajs.react.component.ScalaFn
import japgolly.scalajs.react.hooks.CustomHook
import japgolly.scalajs.react.vdom.VdomNode
import sourcecode.FullName

import scala.annotation.tailrec

// Hook results are suspended to avoid them being executed anytime
opaque type HookResult[A] = () => A

object HookResult:
  inline def apply[A](a: => A): HookResult[A] = () => a

extension (scalafn: ScalaFn.type)
  inline def withFnHooks[P](render: P => HookResult[VdomNode])(using
    name: FullName
  ): ScalaFn.Component[P, CtorType.Props] =
    ScalaFn: props =>
      render(props)()

// extension [P](builder: HookComponentBuilder.ComponentP.First[P])
//   // def monadic(body: P => HookResult[VdomNode]): ScalaFn.Component[P, CtorType.Props] =
//   def apply(body: P => HookResult[VdomNode]): ScalaFn.Component[P, CtorType.Props] =
//     builder.render(p => body(p)())

extension [O](hook: CustomHook[Unit, O])
  inline def toHookResult: HookResult[O] = HookResult(hook.unsafeInit(()))

extension [I, O](hook: CustomHook[I, O])
  inline def toHookResult: I => HookResult[O] = (i: I) => HookResult(hook.unsafeInit(i))

extension (customHook: CustomHook.type)
  def build[I, O](hook: I => HookResult[O]): CustomHook[I, O] =
    CustomHook.unchecked(i => hook(i)())

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
