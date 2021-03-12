// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import japgolly.scalajs.react.Reusability

import scala.reflect.runtime.universe._

trait ReuseSyntax {
  implicit class AnyReuseOps[A]( /*val*/ a: A) { //extends AnyVal {
    def always: Reuse[A] = Reuse.always(a)

    def never: Reuse[A] = Reuse.never(a)

    def curry: Reuse.Curried1[A] = Reuse.curry(a)
  }

  implicit class Fn1ReuseOps[R, B]( /*val*/ fn: R => B) { //extends AnyVal {
    def curry(
      r:        R
    )(implicit
      typeTagR: TypeTag[R],
      reuseR:   Reusability[R]
    ): Reuse[B] = Reuse.curry(r)(fn)
  }

  implicit class Fn2ReuseOps[R, S, B]( /*val*/ fn: (R, S) => B) { //extends AnyVal {
    def curry(
      r:        R
    )(implicit
      typeTagR: TypeTag[R],
      reuseR:   Reusability[R]
    ): Reuse[S => B] = Reuse.curry(r)(fn)
  }

  implicit class Fn3ReuseOps[R, S, T, B]( /*val*/ fn: (R, S, T) => B) { //extends AnyVal {
    def curry(
      r:        R
    )(implicit
      typeTagR: TypeTag[R],
      reuseR:   Reusability[R]
    ): Reuse[(S, T) => B] = Reuse.curry(r)(fn)

    def curry(
      r:        R,
      s:        S
    )(implicit
      typeTagR: TypeTag[(R, S)],
      reuseR:   Reusability[(R, S)]
    ): Reuse[T => B] = Reuse.curry(r, s)(fn)
  }

  implicit class Tuple2ReuseOps[R, S]( /*val*/ t: (R, S)) { //extends AnyVal {
    def curry: Reuse.Curried2[R, S] = Reuse.curry(t._1, t._2)
  }
}

object ReuseSyntax extends ReuseSyntax

// TODO Convert into Reusable ?? Is it possible?

trait Reuse[A] {
  type B

  val value: () => A

  protected val reuseBy: B // We need to store it to combine into tuples when currying.

  protected implicit val typeTag: TypeTag[B]

  // Turn into "(B, B) => Boolean" (will work in JVM)
  protected implicit val reusability: Reusability[B]

  // How to combine the TypeTags and Reusability?
  // def andBy[C](c: C)(implicit typeTagBC: TypeTag[(B, C)], reuseBC: Reusability[(B, C)]): Reuse[A] =
  //   new Reuse[A] {
  //     type B = (Reuse.this.B, C)
  //     val value                                          = Reuse.this.value
  //     implicit protected val typeTag: TypeTag[B]         = typeTagBC
  //     implicit protected val reusability: Reusability[B] = reuseBC
  //   }

  // def orBy[C](c: C)(implicit typeTagC: TypeTag[C], reuseC: Reusability[C]): Reuse[A] = ???

  // // Combine inputs and reusability.
  // def flatMap[C](f: A => Reuse[C]): Reuse[C] =
  //   f(value()).andBy(reuseBy)
}

object Reuse {
  implicit def toA[A](reuseFn: Reuse[A]): A = reuseFn.value()

  implicit def reusability[A]: Reusability[Reuse[A]] =
    Reusability.apply((reuseA, reuseB) =>
      if (reuseA.typeTag == reuseB.typeTag)
        reuseA.reusability.test(reuseA.reuseBy, reuseB.reuseBy.asInstanceOf[reuseA.B]) &&
        reuseB.reusability.test(reuseA.reuseBy.asInstanceOf[reuseB.B], reuseB.reuseBy)
      else false
    )

  def by[A, R](
    reuseByR: R
  )(valueA:   => A)(implicit typeTagR: TypeTag[R], reuseR: Reusability[R]): Reuse[A] =
    new Reuse[A] {
      type B = R

      val value = () => valueA

      protected val reuseBy = reuseByR

      protected val typeTag = typeTagR

      protected val reusability = reuseR
    }

  def always[A](a: A): Reuse[A] = by(())(a)(implicitly[TypeTag[Unit]], Reusability.always)

  def never[A](a: A): Reuse[A] = by(())(a)(implicitly[TypeTag[Unit]], Reusability.never)

  def apply[A](value: => A): Applied[A] = new Applied(value)

  class Applied[A](valueA: => A) {
    val value: A = valueA // TODO Propagate laziness

    def by[R](reuseByR: R)(implicit typeTagR: TypeTag[R], reuseR: Reusability[R]): Reuse[A] =
      Reuse.by(reuseByR)(valueA)

    def always: Reuse[A] = Reuse.by(())(valueA)
  }

  implicit class AppliedFn1Ops[A, R, S, B](aa: Applied[A])(implicit ev: A =:= ((R, S) => B)) {
    // Curry (R, S) => B into (fixed R) + (S => B)
    def apply(
      r:        R
    )(implicit
      typeTagR: TypeTag[R],
      reuseR:   Reusability[R]
    ): Reuse[S => B] =
      Reuse.by(r)(s => ev(aa.value)(r, s))
  }

  implicit class AppliedFn2Ops[A, R, S, T, B](aa: Applied[A])(implicit ev: A =:= ((R, S, T) => B)) {
    // Curry (R, S, T) => B into (fixed R) + ((S, T) => B)
    def apply(
      r:        R
    )(implicit
      typeTagR: TypeTag[R],
      reuseR:   Reusability[R]
    ): Reuse[(S, T) => B] =
      Reuse.by(r)((s, t) => ev(aa.value)(r, s, t))

    // Curry (R, S, T) => B into (fixed (R, S)) + (T => B)
    def apply(
      r:         R,
      s:         S
    )(implicit
      typeTagRS: TypeTag[(R, S)],
      reuseR:    Reusability[(R, S)]
    ): Reuse[T => B] =
      Reuse.by((r, s))(t => ev(aa.value)(r, s, t))
  }

  implicit class ReuseFn1Ops[A, R, B](ra: Reuse[A])(implicit ev: A =:= (R => B)) {
    // We can't use "apply" here since it's ambiguous with "toA" which unwraps the function and applies it.

    // Curry (R, S) => B into (fixed R) + (S => B)
    def curry(
      r:        R
    )(implicit
      typeTagR: TypeTag[(ra.B, R)],
      reuseR:   Reusability[R]
    ): Reuse[B] = {
      implicit val rB = ra.reusability
      Reuse.by((ra.reuseBy, r))(ev(ra.value())(r))
    }
  }

  implicit class ReuseFn2Ops[A, R, S, B](ra: Reuse[A])(implicit ev: A =:= ((R, S) => B)) {
    // We can't use "apply" here since it's ambiguous with "toA" which unwraps the function and applies it.

    // Curry (R, S) => B into (fixed R) + (S => B)
    def curry(
      r:        R
    )(implicit
      typeTagR: TypeTag[(ra.B, R)],
      reuseR:   Reusability[R]
    ): Reuse[S => B] = {
      implicit val rB = ra.reusability
      Reuse.by((ra.reuseBy, r))(s => ev(ra.value())(r, s))
    }
  }

  def curry[R](r: R): Curried1[R] = new Curried1(r)

  def curry[R, S](r: R, s: S): Curried2[R, S] = new Curried2(r, s)

  class Curried1[R](val r: R) {
    def curry[S](s: S): Curried2[R, S] = new Curried2(r, s)
  }

  implicit class Curried1Ops[R](curried: Curried1[R]) {
    // Curry (R, S) => B into (fixed R) +  B
    def apply[B](
      fn:       R => B
    )(implicit
      typeTagR: TypeTag[R],
      reuseR:   Reusability[R]
    ): Reuse[B] =
      Reuse.by(curried.r)(fn(curried.r))

    // Curry (R, S) => B into (fixed R) + (S => B)
    def apply[S, B](
      fn:       (R, S) => B
    )(implicit
      typeTagR: TypeTag[R],
      reuseR:   Reusability[R]
    ): Reuse[S => B] =
      Reuse.by(curried.r)(s => fn(curried.r, s))

    // Curry (R, S, T) => B into (fixed R) + ((S, T) => B)
    def apply[S, T, B](
      fn:       (R, S, T) => B
    )(implicit
      typeTagR: TypeTag[R],
      reuseR:   Reusability[R]
    ): Reuse[(S, T) => B] =
      Reuse.by(curried.r)((s, t) => fn(curried.r, s, t))
  }

  class Curried2[R, S](val r: R, val s: S) {
    def apply[A, T, B](
      valueA:   => A
    )(implicit
      ev:       A =:= ((R, S, T) => B),
      typeTagR: TypeTag[(R, S)],
      reuseR:   Reusability[(R, S)]
    ): Reuse[T => B] =
      Reuse.by((r, s))(t => ev(valueA)(r, s, t))
  }

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

    // Currying

    def format(units: String, q: Double): String = s"$q $units"

    val cexample1                           = Reuse(format _)("meters")
    val cexample1a: Reuse[Double => String] = cexample1

    def format2(units: String, prefix: String, q: Double): String =
      s"$prefix $q $units"

    val cexample2 = Reuse(format2 _)("meters")

    val cexample3 = Reuse(format2 _)("meters", "Length:")

    val cexample4 = Reuse(format2 _)("meters").curry("Length:")

    val cexample5a = Reuse(format2 _)("meters")
    val cexample5  = cexample5a.curry("Length:")

    val cexample6 = Reuse.curry("meters")(format _)

    val cexample7 = Reuse.curry("meters", "Length:")(format2 _)

    import ReuseSyntax._

    val cexample8 = (format _).curry("meters")

    val cexample9 = (format2 _).curry("meters").curry("Length:")

    val cexample10 = (format2 _).curry("meters", "Length:")

    val cexample11 = "meters".curry(format _)

    val cexample12 = "meters".curry(format2 _).curry("Length:")

    val cexample13 = ("meters", "Length:").curry(format2 _)

    implicit val reuseDouble = Reusability.double(0.1)

    val cexample14 = ("meters", "Length:").curry(format2 _).curry(10.0)
  }
}
