// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils.reuse

import japgolly.scalajs.react.Reusability
import scala.reflect.ClassTag

/*
 * Wraps a (lazy) `value` of type `A` and an associated `reuseBy` of a hidden type `B`,
 * delegating `Reusability` to `Reusability[B]`.
 *
 * In other words, delegates existential `Reusability` of instances of `A` to an existing
 * universal `Reusability` of `B`, while associating instances of `A` to instances of `B`.
 *
 * It's particularly useful to provide `Reusability` for functions or VDOM elements.
 *
 * When used for functions, it differs from `scalajs-react`'s `Reusable.fn` mainly in that
 * the reference equality of the wrapped function is not checked: `Reusability` is
 * computed solely based on the provided `reuseBy` value. This allows using inline idioms
 * like:
 *
 *   `Component(Reuse.currying(props).in( (props, text: String) => ...: VdomNode)`
 *
 * in which `Component` would expect a `Reuse[String => VdomNode]` as a parameter. In this
 * case, the wrapped `String => VdomNode` function is reused as long as `props` can be reused.
 *
 * A number of convenience constructors, methods and impliits are provided in order to
 * support a wide variety of use cases. Notably, `(A, B, ...) ==> Z` is a type alias for
 * `Reusable[(A, B, ...) => Z]`.
 *
 */
trait Reuse[A] {
  type B

  val value: () => A

  protected val reuseBy: B // We need to store it to combine into tuples when currying.

  protected implicit val ClassTag: ClassTag[B]

  protected implicit val reusability: Reusability[B]

  def map[C](f: A => C): Reuse[C] = Reuse.by(reuseBy)(f(value()))
}

object Reuse extends ReusableInterop {
  implicit def toA[A](reuseFn: Reuse[A]): A = reuseFn.value()

  implicit def reusability[A]: Reusability[Reuse[A]] =
    Reusability.apply((reuseA, reuseB) =>
      if (reuseA.ClassTag == reuseB.ClassTag)
        reuseA.reusability.test(reuseA.reuseBy, reuseB.reuseBy.asInstanceOf[reuseA.B]) &&
        reuseB.reusability.test(reuseA.reuseBy.asInstanceOf[reuseB.B], reuseB.reuseBy)
      else false
    )

  def by[A, R](reuseByR: R) = new AppliedBy(reuseByR)

  def always[A](a: A): Reuse[A] = by(())(a)(implicitly[ClassTag[Unit]], Reusability.always)

  def never[A](a: A): Reuse[A] = by(())(a)(implicitly[ClassTag[Unit]], Reusability.never)

  def apply[A](value: => A): Applied[A] = new Applied(value)

  def currying[R](r: R): Curried1[R] = new Curried1(r)

  def currying[R, S](r: R, s: S): Curried2[R, S] = new Curried2(r, s)

  def currying[R, S, T](r: R, s: S, t: T): Curried3[R, S, T] = new Curried3(r, s, t)

  class AppliedBy[R](reuseByR: R) {
    def apply[A, S, T, B](
      fn:                 (S, T) => B
    )(implicit classTagR: ClassTag[R], reuseR: Reusability[R]): (S, T) ==> B =
      apply(fn.tupled)

    def apply[A, S, T, U, B](
      fn:                 (S, T, U) => B
    )(implicit classTagR: ClassTag[R], reuseR: Reusability[R]): (S, T, U) ==> B =
      apply(fn.tupled)

    def apply[A](valueA: => A)(implicit classTagR: ClassTag[R], reuseR: Reusability[R]): Reuse[A] =
      new Reuse[A] {
        type B = R

        val value = () => valueA

        protected val reuseBy = reuseByR

        protected val ClassTag = classTagR

        protected val reusability = reuseR
      }
  }

  class Applied[A](valueA: => A) {
    val value: A = valueA // TODO Propagate laziness

    def by[R](reuseByR: R)(implicit classTagR: ClassTag[R], reuseR: Reusability[R]): Reuse[A] =
      Reuse.by(reuseByR)(valueA)

    def always: Reuse[A] = Reuse.by(())(valueA)
  }

  implicit class AppliedFn2Ops[A, R, S, B](aa: Applied[A])(implicit ev: A =:= ((R, S) => B)) {
    // Curry (R, S) => B into (fixed R) + (S => B)
    def apply(
      r:         R
    )(implicit
      classTagR: ClassTag[R],
      reuseR:    Reusability[R]
    ): Reuse[S => B] =
      Reuse.by(r)(s => ev(aa.value)(r, s))

    def apply(
      r:         R,
      s:         S
    )(implicit
      classTagR: ClassTag[(R, S)],
      reuseR:    Reusability[(R, S)]
    ): Reuse[B] =
      Reuse.by((r, s))(ev(aa.value)(r, s))
  }

  implicit class AppliedFn3Ops[A, R, S, T, B](aa: Applied[A])(implicit ev: A =:= ((R, S, T) => B)) {
    // Curry (R, S, T) => B into (fixed R) + ((S, T) => B)
    def apply(
      r:         R
    )(implicit
      classTagR: ClassTag[R],
      reuseR:    Reusability[R]
    ): Reuse[(S, T) => B] =
      Reuse.by(r)((s, t) => ev(aa.value)(r, s, t))

    // Curry (R, S, T) => B into (fixed (R, S)) + (T => B)
    def apply(
      r:          R,
      s:          S
    )(implicit
      classTagRS: ClassTag[(R, S)],
      reuseR:     Reusability[(R, S)]
    ): Reuse[T => B] =
      Reuse.by((r, s))(t => ev(aa.value)(r, s, t))

    def apply(
      r:          R,
      s:          S,
      t:          T
    )(implicit
      classTagRS: ClassTag[(R, S, T)],
      reuseR:     Reusability[(R, S, T)]
    ): Reuse[B] =
      Reuse.by((r, s, t))(ev(aa.value)(r, s, t))
  }

  implicit class AppliedFn4Ops[A, R, S, T, U, B](aa: Applied[A])(implicit
    ev:                                              A =:= ((R, S, T, U) => B)
  ) {
    // Curry (R, S, T, U) => B into (fixed R) + ((S, T, U) => B)
    def apply(
      r:         R
    )(implicit
      classTagR: ClassTag[R],
      reuseR:    Reusability[R]
    ): Reuse[(S, T, U) => B] =
      Reuse.by(r)((s, t, u) => ev(aa.value)(r, s, t, u))

    // Curry (R, S, T, U) => B into (fixed (R, S)) + ((T, U) => B)
    def apply(
      r:          R,
      s:          S
    )(implicit
      classTagRS: ClassTag[(R, S)],
      reuseR:     Reusability[(R, S)]
    ): Reuse[(T, U) => B] =
      Reuse.by((r, s))((t, u) => ev(aa.value)(r, s, t, u))

    def apply(
      r:          R,
      s:          S,
      t:          T
    )(implicit
      classTagRS: ClassTag[(R, S, T)],
      reuseR:     Reusability[(R, S, T)]
    ): Reuse[U => B] =
      Reuse.by((r, s, t))(u => ev(aa.value)(r, s, t, u))

    def apply(
      r:          R,
      s:          S,
      t:          T,
      u:          U
    )(implicit
      classTagRS: ClassTag[(R, S, T, U)],
      reuseR:     Reusability[(R, S, T, U)]
    ): Reuse[B] =
      Reuse.by((r, s, t, u))(ev(aa.value)(r, s, t, u))
  }

  implicit class ReuseFn1Ops[A, R, B](ra: Reuse[A])(implicit ev: A =:= (R => B)) {
    // Curry (R, S) => B into (fixed R) + (S => B)
    def curry(
      r:         R
    )(implicit
      classTagR: ClassTag[(ra.B, R)],
      reuseR:    Reusability[R]
    ): Reuse[B] = {
      implicit val rB = ra.reusability
      Reuse.by((ra.reuseBy, r))(ev(ra.value())(r))
    }
  }

  implicit class ReuseFn2Ops[A, R, S, B](ra: Reuse[A])(implicit ev: A =:= ((R, S) => B)) {
    // Curry (R, S) => B into (fixed R) + (S => B)
    def curry(
      r:         R
    )(implicit
      classTagR: ClassTag[(ra.B, R)],
      reuseR:    Reusability[R]
    ): Reuse[S => B] = {
      implicit val rB = ra.reusability
      Reuse.by((ra.reuseBy, r))(s => ev(ra.value())(r, s))
    }
  }

  implicit def tupledReuseFn2[A, R, S, B](ra: Reuse[A])(implicit
    ev:                                       A =:= ((R, S) => B)
  ): (R, S) ==> B =
    ra.map(f => ev(f).tupled)

  implicit class ReuseFn2TupledOps[A, R, S, B](ra: Reuse[A])(implicit ev: A =:= (((R, S)) => B)) {
    def curry(
      r:         R
    )(implicit
      classTagR: ClassTag[(ra.B, R)],
      reuseR:    Reusability[R]
    ): Reuse[S => B] = {
      implicit val rB = ra.reusability
      Reuse.by((ra.reuseBy, r))(s => ev(ra.value())((r, s)))

    }
  }

  class Curried1[R](val r: R) {
    def and[S](s: S): Curried2[R, S] = new Curried2(r, s)

    // Curry (R, S) => B into (fixed R) +  B
    def in[B](
      fn:        R => B
    )(implicit
      classTagR: ClassTag[R],
      reuseR:    Reusability[R]
    ): Reuse[B] =
      Reuse.by(r)(fn(r))

    // Curry (R, S) => B into (fixed R) + (S => B)
    def in[S, B](
      fn:        (R, S) => B
    )(implicit
      classTagR: ClassTag[R],
      reuseR:    Reusability[R]
    ): Reuse[S => B] =
      Reuse.by(r)(s => fn(r, s))

    // Curry (R, S, T) => B into (fixed R) + ((S, T) => B)
    def in[S, T, B](
      fn:        (R, S, T) => B
    )(implicit
      classTagR: ClassTag[R],
      reuseR:    Reusability[R]
    ): Reuse[(S, T) => B] =
      Reuse.by(r)((s, t) => fn(r, s, t))
  }

  class Curried2[R, S](val r: R, val s: S) {
    def in[B](
      fn:        (R, S) => B
    )(implicit
      classTagR: ClassTag[(R, S)],
      reuseR:    Reusability[(R, S)]
    ): Reuse[B] =
      Reuse.by((r, s))(fn(r, s))

    def in[T, B](
      fn:        (R, S, T) => B
    )(implicit
      classTagR: ClassTag[(R, S)],
      reuseR:    Reusability[(R, S)]
    ): Reuse[T => B] =
      Reuse.by((r, s))(t => fn(r, s, t))
  }

  class Curried3[R, S, T](val r: R, val s: S, val t: T) {
    def in[B](
      fn:        (R, S, T) => B
    )(implicit
      classTagR: ClassTag[(R, S, T)],
      reuseR:    Reusability[(R, S, T)]
    ): Reuse[B] =
      Reuse.by((r, s, t))(fn(r, s, t))
  }

  object Examples {
    val propsUnits: String = "meters"

    def decorate(q: Double): String = s"$q $propsUnits"

    // Basic reuse

    val example1: Double ==> String =
      Reuse((q: Double) => s"$q $propsUnits").by(propsUnits)

    val example2: Double ==> String =
      Reuse.by(propsUnits)(q => s"$q $propsUnits")

    val example3: Double ==> String =
      Reuse.by(propsUnits)((q: Double) => s"$q $propsUnits")

    val example4: Double ==> String =
      Reuse(decorate _).by(propsUnits)

    val example5: Double ==> String =
      Reuse.by(propsUnits)(decorate _)

    // Currying

    def format(units: String, q: Double): String = s"$q $units"

    def format2(units: String, prefix: String, q: Double): String =
      s"$prefix $q $units"

    val cexample1: Double ==> String  =
      Reuse(format _)("meters")
    val cexample1a: Double ==> String =
      cexample1

    val cexample2: (String, Double) ==> String =
      Reuse(format2 _)("meters")

    val cexample3: Double ==> String =
      Reuse(format2 _)("meters", "Length:")

    val cexample4: Double ==> String =
      Reuse(format2 _)("meters").curry("Length:")

    val cexample5a: (String, Double) ==> String =
      Reuse(format2 _)("meters")
    val cexample5: Double ==> String            =
      cexample5a.curry("Length:")

    val cexample6: Double ==> String =
      Reuse.currying("meters").in(format _)

    val cexample7: Double ==> String =
      Reuse.currying("meters", "Length:").in(format2 _)

    val cexample8: Double ==> String =
      (format _).reuseCurrying("meters")

    val cexample9: Double ==> String =
      (format2 _).reuseCurrying("meters").curry("Length:")

    val cexample10: Double ==> String =
      (format2 _).reuseCurrying("meters", "Length:")

    val cexample11: Double ==> String =
      "meters".curryReusing.in(format _)

    val cexample12: Double ==> String =
      "meters".curryReusing.in(format2 _).curry("Length:")

    val cexample13: Double ==> String =
      ("meters", "Length:").curryReusing.in(format2 _)

    implicit val reuseDouble = Reusability.double(0.1)

    val cexample14: Reuse[String] =
      ("meters", "Length:").curryReusing.in(format2 _).curry(10.0)
  }
}

trait ReusableInterop {
  import japgolly.scalajs.react._

  implicit def toReusableFn1[A, R, B](ra: Reuse[A])(implicit ev: A =:= (R => B)): R ~=> B =
    Reusable.implicitly(new Fn1(ra.map(ev)))

  class Fn1[R, B](val f: Reuse[R => B]) extends Function1[R, B] {
    def apply(r: R): B = f.value()(r)
  }
  implicit def reusablityFn1[R, B]: Reusability[Fn1[R, B]] = Reusability.by(_.f)
}
