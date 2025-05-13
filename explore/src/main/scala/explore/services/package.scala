// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.Monad
import cats.syntax.all.*

private def drain[F[_]: Monad, A, Id, R](
  fetch:      Option[Id] => F[R],
  getList:    R => List[A],
  getHasMore: R => Boolean,
  getId:      A => Id
): F[List[A]] = {
  def go(id: Option[Id], accum: List[A]): F[List[A]] =
    fetch(id).flatMap(result =>
      val list = getList(result)
      if (getHasMore(result)) go(list.lastOption.map(getId), list)
      // Fetching with offset includes the offset, so .dropRight(1) ensures we don't include it twice.
      else (accum.dropRight(1) ++ list).pure[F]
    )

  go(none, List.empty)
}
