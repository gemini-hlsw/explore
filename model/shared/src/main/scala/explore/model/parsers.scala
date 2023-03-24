// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.parse.Numbers.digit
import cats.parse.Numbers.digits
import cats.parse.Parser.char
import cats.parse.Rfc5234.sp
import cats.parse.*
import cats.syntax.all.*
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.refineV
import lucuma.core.math.parser.AngleParsers
import lucuma.core.parser.MiscParsers
import lucuma.core.util.TimeSpan

import java.time.Duration

trait parsers:
  val colonOrSpace: Parser[Unit] = MiscParsers.colon | sp

  val durationHM: Parser[TimeSpan] =
    (digits ~ MiscParsers.colon.void.? ~ AngleParsers.minutes.?)
      .mapFilter { case ((h, _), m) =>
        MiscParsers
          .catchNFE[(String, Option[Int]), Duration] { case (h, m) =>
            Duration.ofMinutes(h.toLong * 60 + m.foldMap(_.toInt))
          }(h, m)
          .map(TimeSpan.unsafeFromDuration)

      }
      .withContext("duration_hm")

  val seconds =
    // allow any amount of decimals but only use 6
    (AngleParsers.minutes ~ char('.').? ~ digit.rep(1, 3).?.string ~ digit
      .rep(1, 3)
      .?
      .string ~ digit.rep.?)
      .map { case ((((s, _), d1), d2), _) =>
        (s, if (d1.isEmpty) 0 else d1.padTo(3, '0').toInt, if (d2.isEmpty) 0 else d2.toInt)
      }
      .withContext("seconds")

  val durationHMS: Parser[TimeSpan] =
    (digits ~ MiscParsers.colon.void.? ~ AngleParsers.minutes.? ~ MiscParsers.colon.void.? ~ seconds.?)
      .mapFilter { case ((((h, _), m), _), ss) =>
        MiscParsers
          .catchNFE[(String, Option[Int], Option[Int], Option[Int]), Duration] {
            case (h, m, s, ms) =>
              Duration
                .ofSeconds(h.toLong * 3600 + m.foldMap(_.toInt) * 60 + s.orEmpty)
                .plusMillis(ms.orEmpty)
          }(h, m, ss.map(_._1), ss.map(_._2))
          .map(TimeSpan.unsafeFromDuration)
      }
      .withContext("duration_hm")

object parsers extends parsers
