// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import io.circe.Decoder
import lucuma.core.model.NonNegDuration
import lucuma.core.model.implicits.*
import monocle.Focus
import monocle.Lens
import org.typelevel.cats.time.instances.zoneddatetime.*

import java.time.Duration
import java.time.ZonedDateTime

case class TimingWindowEntry private (
  id:            Int,
  startsOn:      ZonedDateTime,
  forever:       Boolean,
  repeatPeriod:  Option[Int] = None,
  repeatForever: Option[Boolean] = None,
  repeat:        Boolean = false,
  repeatTimes:   Option[Int] = None,
  remainOpenFor: Option[NonNegDuration] = None,
  closeOn:       Option[ZonedDateTime] = None
) derives Eq {
  def toCloseOn(closeOn: ZonedDateTime): TimingWindowEntry =
    copy(forever = false,
         repeatPeriod = None,
         repeatForever = None,
         repeatTimes = None,
         remainOpenFor = None,
         closeOn = closeOn.some
    )

  def toForever: TimingWindowEntry =
    copy(forever = true,
         repeatPeriod = None,
         repeatForever = None,
         repeatTimes = None,
         remainOpenFor = None,
         closeOn = None
    )

  def toRemainOpen: TimingWindowEntry =
    copy(forever = false,
         repeatPeriod = None,
         repeatForever = None,
         repeatTimes = None,
         remainOpenFor = Some(NonNegDuration.unsafeFrom(Duration.ofDays(1))),
         closeOn = None
    )
}

object TimingWindowEntry:
  val startsOn: Lens[TimingWindowEntry, ZonedDateTime] = Focus[TimingWindowEntry](_.startsOn)

  val closeOn: Lens[TimingWindowEntry, Option[ZonedDateTime]] = Focus[TimingWindowEntry](_.closeOn)

  val remainOpenFor: Lens[TimingWindowEntry, Option[NonNegDuration]] =
    Focus[TimingWindowEntry](_.remainOpenFor)

  val repeatTimes: Lens[TimingWindowEntry, Option[Int]] =
    Focus[TimingWindowEntry](_.repeatTimes)

  val repeat: Lens[TimingWindowEntry, Boolean] =
    Focus[TimingWindowEntry](_.repeat)

  def forever(id: Int, startsOn: ZonedDateTime): TimingWindowEntry =
    new TimingWindowEntry(id, startsOn.withSecond(0).withNano(0), true)
