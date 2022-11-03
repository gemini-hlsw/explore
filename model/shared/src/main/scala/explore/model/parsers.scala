// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.parse.Numbers.digit
import cats.parse.Numbers.digits
import cats.parse.*
import cats.syntax.all.*
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.refineV
import lucuma.core.model.NonNegDuration
import lucuma.core.parser.MiscParsers

import java.time.Duration

trait parsers:
  val durationHM: Parser[NonNegDuration] =
    (digits ~ MiscParsers.colon.void.? ~ digit.rep(1, 2).string.?)
      .mapFilter { case ((h, _), m) =>
        MiscParsers
          .catchNFE[(String, Option[String]), Duration] { case (h, m) =>
            Duration.ofMinutes(h.toLong * 60 + m.foldMap(_.toInt))
          }(h, m)
          .map(NonNegDuration.unsafeFrom)
      }
      .withContext("duration_hm")

object parsers extends parsers
