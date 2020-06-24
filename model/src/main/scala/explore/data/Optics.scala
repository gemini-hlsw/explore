// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.data

import cats._
import cats.implicits._
import monocle._

object Optics {
  def filteredTraversal[T](predicate: T => Boolean): Traversal[List[T], T] =
    new Traversal[List[T], T] {
      def modifyF[F[_]: Applicative](f: T => F[T])(s: List[T]): F[List[T]] =
        s.map(v => if (predicate(v)) f(v) else v.pure[F]).sequence
    }

  // Lenses must be disjoint (not overlap), or the result will be unlawful.
  // See https://github.com/optics-dev/Monocle/issues/545
  def disjointZip[S, A, B](l1: Lens[S, A], l2: Lens[S, B]): Lens[S, (A, B)] =
    Lens((s: S) => (l1.get(s), l2.get(s)))((ab: (A, B)) =>
      (s: S) => l2.set(ab._2)(l1.set(ab._1)(s))
    )

  def disjointZip[S, A, B, C](l1: Lens[S, A], l2: Lens[S, B], l3: Lens[S, C]): Lens[S, (A, B, C)] =
    Lens((s: S) => (l1.get(s), l2.get(s), l3.get(s)))((abc: (A, B, C)) =>
      (s: S) => l3.set(abc._3)(l2.set(abc._2)(l1.set(abc._1)(s)))
    )
}
