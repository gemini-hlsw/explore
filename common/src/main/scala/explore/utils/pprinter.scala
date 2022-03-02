// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import cats.syntax.all._
import scala.scalajs.LinkingInfo
import pprint.PPrinter
import org.typelevel.log4cats.Logger
import cats.Monad

trait pprinter {
  def apply(x: Any): String
}

class devPPrinter extends pprinter {
  private val pprinterInstance: PPrinter =
    PPrinter(defaultHeight = 200, colorApplyPrefix = fansi.Color.Blue)

  def apply(x: Any): String =
    pprinterInstance(x, initialOffset = 4).toString
}

class prodPPrinter extends pprinter {
  def apply(x: Any): String = x.toString
}

object pprinter {
  private val printer: pprinter =
    if (LinkingInfo.developmentMode) new devPPrinter else new prodPPrinter

  def apply(x: Any): String = printer.apply(x)

  def error[F[_]](x: Any)(implicit logger: Logger[F]): F[Unit] =
    logger.error(apply(x))

  def error[F[_]: Monad](message: String, x: Any)(implicit logger: Logger[F]): F[Unit] =
    logger.error(message) >> logger.error(apply(x))

  def warn[F[_]](x: Any)(implicit logger: Logger[F]): F[Unit] =
    logger.warn(apply(x))

  def warn[F[_]: Monad](message: String, x: Any)(implicit logger: Logger[F]): F[Unit] =
    logger.warn(message) >> logger.warn(apply(x))

  def info[F[_]](x: Any)(implicit logger: Logger[F]): F[Unit] =
    logger.info(apply(x))

  def info[F[_]: Monad](message: String, x: Any)(implicit logger: Logger[F]): F[Unit] =
    logger.info(message) >> logger.info(apply(x))

  def debug[F[_]](x: Any)(implicit logger: Logger[F]): F[Unit] =
    logger.debug(apply(x))

  def debug[F[_]: Monad](message: String, x: Any)(implicit logger: Logger[F]): F[Unit] =
    logger.debug(message) >> logger.debug(apply(x))

  def trace[F[_]](x: Any)(implicit logger: Logger[F]): F[Unit] =
    logger.trace(apply(x))

  def trace[F[_]: Monad](message: String, x: Any)(implicit logger: Logger[F]): F[Unit] =
    logger.trace(message) >> logger.trace(apply(x))
}
