// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.data.NonEmptyList
import cats.effect.Async
import cats.syntax.all.*
import fs2.Stream
import fs2.data.csv.*
import fs2.io.file.*
import fs2.text

trait SpectroscopyModesMatrixPlatform extends SpectroscopyModesMatrixDecoders {

  def loadMatrix[F[_]: Async: Files](path: Path): F[List[SpectroscopyModeRow]] =
    Files[F]
      .readAll(path)
      .through(text.utf8.decode)
      .through(decodeUsingHeaders[NonEmptyList[SpectroscopyModeRow]]())
      .flatMap(l => Stream.emits(l.toList))
      .zipWithIndex
      .map { case (r, i) =>
        r.copy(id = i.toInt)
      }
      .compile
      .toList

  def apply[F[_]: Async: Files](path: Path): F[SpectroscopyModesMatrix] =
    loadMatrix(path).map(SpectroscopyModesMatrix(_))
}
