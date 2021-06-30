// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.effect.Concurrent
import cats.syntax.all._
import fs2._
import fs2.data.csv._

trait SpectroscopyModesMatrixPlatform extends SpectroscopyModesMatrixDecoders {
  def loadMatrix[F[_]: Concurrent](s: Stream[F, String]): F[List[SpectroscopyModeRow]] =
    s
      .through(decodeUsingHeaders[SpectroscopyModeRow]())
      .compile
      .toList

  def apply[F[_]: Concurrent](s: Stream[F, String]): F[SpectroscopyModesMatrix] =
    loadMatrix(s).map(SpectroscopyModesMatrix(_))

}
