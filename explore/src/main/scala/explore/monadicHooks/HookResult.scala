// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.monadicHooks

import cats.Monad
import scala.annotation.tailrec
import japgolly.scalajs.react.vdom.VdomNode

opaque type HookResult[A] = () => A

object HookResult:
  def apply[A](a: => A): HookResult[A] = () => a

given Conversion[HookResult[VdomNode], VdomNode] with
  inline def apply(hr: HookResult[VdomNode]): VdomNode = hr()

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
