// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.parse.*
import cats.parse.Numbers.digit
import cats.parse.Numbers.digits
import cats.parse.Parser.char
import cats.parse.Rfc5234.sp
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
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

  // Duration in the form of secs.milllis
  val durationMs: Parser0[TimeSpan] =
    (digits.? ~ (char('.') ~ digit.rep(1, 3).string ~ digits.?).?)
      .map {
        case (s, Some(((_, ms), _))) =>
          val seconds = s.foldMap(_.toLong)
          val millis  = ms.padTo(3, '0').toLong
          TimeSpan.unsafeFromDuration(Duration.ofSeconds(seconds).plusMillis(millis))
        case (s, _)                  =>
          val seconds = s.foldMap(_.toLong)
          TimeSpan.unsafeFromDuration(Duration.ofSeconds(seconds))
      }
      .withContext("duration_s")
      
  // Parse a non-negative integer
  val nonNegInt: Parser[NonNegInt] =
    digits
      .mapFilter { s =>
        MiscParsers
          .catchNFE[String, Int](_.toInt)(s)
          .flatMap(i => NonNegInt.from(i).toOption)
      }
      .withContext("non_neg_int")

object parsers extends parsers
