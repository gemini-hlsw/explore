// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.util

import cats._
import cats.implicits._
import monocle._

object Monocle {
  def filteredTraversal[T](predicate: T => Boolean): Traversal[List[T], T] =
    new Traversal[List[T], T] {
      def modifyF[F[_]: Applicative](f: T => F[T])(s: List[T]): F[List[T]] =
        s.map(v => if (predicate(v)) f(v) else v.pure[F]).sequence
    }
}
