// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.*
import fs2.data.csv.*

trait ModesMatrixPlatform extends ModesMatrixDecoders {
  def loadMatrix[F[_]: Concurrent](s: Stream[F, String]): F[List[ModeRow]] =
    s
      .through(decodeUsingHeaders[ModeRow]())
      .compile
      .toList

  def apply[F[_]: Concurrent](s: Stream[F, String]): F[ModesMatrix] =
    loadMatrix(s).map(ModesMatrix(_))

}
