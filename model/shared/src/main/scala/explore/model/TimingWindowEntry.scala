// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import org.typelevel.cats.time.instances.zoneddatetime.*

import java.time.ZonedDateTime
import io.circe.Decoder

case class TimingWindowEntry(
  id:            Int,
  startsOn:      ZonedDateTime,
  repeatPeriod:  Int,
  repeatForever: Boolean,
  repeatTimes:   Int,
  remainOpenFor: Int,
  forever:       Boolean,
  closeOn:       ZonedDateTime
) derives Eq,
      Decoder
