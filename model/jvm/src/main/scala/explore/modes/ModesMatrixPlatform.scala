// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.data.csv.*
import fs2.io.file.*
import fs2.text

trait ModesMatrixPlatform extends ModesMatrixDecoders {

  def loadMatrix[F[_]: Concurrent: Files](path: Path): F[List[ModeRow]] =
    Files[F]
      .readAll(path)
      .through(text.utf8.decode)
      .through(decodeUsingHeaders[ModeRow]())
      .compile
      .toList

  def apply[F[_]: Concurrent: Files](path: Path): F[ModesMatrix] =
    loadMatrix(path).map(ModesMatrix(_))
}
