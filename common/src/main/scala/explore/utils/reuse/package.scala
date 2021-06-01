package explore.utils

import japgolly.scalajs.react.Reusability
import scala.reflect.ClassTag

package object reuse extends ReuseSyntax {
  type ==>[A, B] = Reuse[A => B]
}

package reuse {
  trait ReuseSyntax                      {
    implicit class AnyReuseOps[A]( /*val*/ a: A) { //extends AnyVal {
      def reuseAlways: Reuse[A] = Reuse.always(a)

      def reuseNever: Reuse[A] = Reuse.never(a)

      def curryReusing: Reuse.Curried1[A] = Reuse.currying(a)
    }

    implicit class Tuple2ReuseOps[R, S]( /*val*/ t: (R, S)) { //extends AnyVal {
      def curryReusing: Reuse.Curried2[R, S] = Reuse.currying(t._1, t._2)
    }

    implicit class Fn1ReuseOps[R, B]( /*val*/ fn: R => B) { //extends AnyVal {
      def reuseCurrying(
        r:         R
      )(implicit
        classTagR: ClassTag[R],
        reuseR:    Reusability[R]
      ): Reuse[B] = Reuse.currying(r).in(fn)
    }

    implicit class Fn2ReuseOps[R, S, B]( /*val*/ fn: (R, S) => B) { //extends AnyVal {
      def reuseCurrying(
        r:         R
      )(implicit
        classTagR: ClassTag[R],
        reuseR:    Reusability[R]
      ): Reuse[S => B] = Reuse.currying(r).in(fn)

      def reuseCurrying(
        r:         R,
        s:         S
      )(implicit
        classTagR: ClassTag[(R, S)],
        reuseR:    Reusability[(R, S)]
      ): Reuse[B] = Reuse.currying(r, s).in(fn)
    }

    implicit class Fn3ReuseOps[R, S, T, B]( /*val*/ fn: (R, S, T) => B) { //extends AnyVal {
      def reuseCurrying(
        r:         R
      )(implicit
        classTagR: ClassTag[R],
        reuseR:    Reusability[R]
      ): Reuse[(S, T) => B] = Reuse.currying(r).in(fn)

      def reuseCurrying(
        r:         R,
        s:         S
      )(implicit
        classTagR: ClassTag[(R, S)],
        reuseR:    Reusability[(R, S)]
      ): Reuse[T => B] = Reuse.currying(r, s).in(fn)
    }

  }

}
