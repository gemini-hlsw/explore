// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import io.circe.Decoder
import lucuma.core.util.TimeSpan
import lucuma.odb.json.time.decoder.given_Decoder_TimeSpan

opaque type ProgramTime = TimeSpan

object ProgramTime:
  inline def apply(ts: TimeSpan): ProgramTime                  = ts
  extension (pt:       ProgramTime) inline def value: TimeSpan = pt

  given (using eq: Eq[TimeSpan]): Eq[ProgramTime] = eq

  // This decoder is meant to decode a `CategorizedTime` with just a `program` time in it.
  // If we need more information from CategorizedTime, we can switch to using
  // lucuma.core.model.sequence.CategorizedTime instead of ProgramTime.
  given Decoder[ProgramTime] =
    Decoder.instance(_.get[TimeSpan]("program")(using given_Decoder_TimeSpan))
