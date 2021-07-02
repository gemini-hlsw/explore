// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.effect.Async
import cats.syntax.all._
import fs2.data.csv._
import fs2.io.file._
import fs2.text

import java.nio.file.Path

trait SpectroscopyModesMatrixPlatform extends SpectroscopyModesMatrixDecoders {

  def loadMatrix[F[_]: Async](path: Path): F[List[SpectroscopyModeRow]] =
    Files[F]
      .readAll(path, 1024)
      .through(text.utf8Decode)
      .through(decodeUsingHeaders[SpectroscopyModeRow]())
      .zipWithIndex
      .map { case (r, i) =>
        r.copy(id = i)
      }
      .compile
      .toList

  def apply[F[_]: Async](path: Path): F[SpectroscopyModesMatrix] =
    loadMatrix(path).map(SpectroscopyModesMatrix(_))
}
