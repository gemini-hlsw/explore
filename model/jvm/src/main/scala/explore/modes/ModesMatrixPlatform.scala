// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.effect.Async
import cats.syntax.all._
import fs2.data.csv._
import fs2.io.file._
import fs2.text

trait ModesMatrixPlatform extends ModesMatrixDecoders {

  def loadMatrix[F[_]: Async](path: Path): F[List[ModeRow]] =
    Files[F]
      .readAll(path)
      .through(text.utf8.decode)
      .through(decodeUsingHeaders[ModeRow]())
      .compile
      .toList

  def apply[F[_]: Async](path: Path): F[ModesMatrix] = loadMatrix(path).map(ModesMatrix(_))
}
