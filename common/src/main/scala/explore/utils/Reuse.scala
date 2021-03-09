// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import japgolly.scalajs.react.Reusability

import scala.reflect.runtime.universe._

/*
 * This is a fledgling idea for a mechanism to provide Reusability for functions.
 * It's not used yet, will develop in future PR.
 */
trait Reuse[A] {
  type B

  val value: A

  protected val reuseBy: B

  protected val typeTag: TypeTag[B]

  protected val reusability: Reusability[B]
}

object Reuse {
  implicit def toA[A](reuseFn: Reuse[A]): A = reuseFn.value

  implicit def reusability[A]: Reusability[Reuse[A]] =
    Reusability.apply((reuseA, reuseB) =>
      if (reuseA.typeTag == reuseB.typeTag)
        reuseA.reusability.test(reuseA.reuseBy, reuseB.reuseBy.asInstanceOf[reuseA.B])
      else false
    )

  def by[A, R](
    reuseByR: R
  )(valueA:   A)(implicit typeTagR: TypeTag[R], reuseR: Reusability[R]): Reuse[A] =
    new Reuse[A] {
      type B = R

      val value = valueA

      protected val reuseBy = reuseByR

      protected val typeTag = typeTagR

      protected val reusability = reuseR
    }

  def apply[A](value: A): Applied[A] = Applied(value)

  case class Applied[A](valueA: A) {
    def by[R](reuseByR: R)(implicit typeTagR: TypeTag[R], reuseR: Reusability[R]): Reuse[A] =
      Reuse.by(reuseByR)(valueA)
  }

  // TODO Convert into Reusable ??

  object Examples {
    val propsUnits: String = "meters"

    val example1: Double ==> String =
      Reuse((q: Double) => s"$q $propsUnits").by(propsUnits)

    val example2: Double ==> String =
      Reuse.by(propsUnits)(q => s"$q $propsUnits")

    val example3 = Reuse.by(propsUnits)((q: Double) => s"$q $propsUnits")

    def decorate(q: Double): String = s"$q $propsUnits"

    val example4 = Reuse(decorate _).by(propsUnits)

    val example5 = Reuse.by(propsUnits)(decorate _)
  }
}
