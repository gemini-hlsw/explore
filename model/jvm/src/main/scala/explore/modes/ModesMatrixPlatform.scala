package explore.modes

import cats.effect.Async
import cats.syntax.all._
import fs2.data.csv._
import fs2.io.file._
import fs2.text

import java.nio.file.Path

trait ModesMatrixPlatform extends ModesMatrixDecoders {

  def loadMatrix[F[_]: Async](path: Path): F[List[ModeRow]] =
    Files[F]
      .readAll(path, 1024)
      .through(text.utf8Decode)
      .through(decodeUsingHeaders[ModeRow]())
      .compile
      .toList

  def apply[F[_]: Async](path: Path): F[ModesMatrix[F]] = loadMatrix(path).map(rows =>
    new ModesMatrix[F] {
      def matrix = rows
    }
  )
}
