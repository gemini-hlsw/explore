// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.cats.*
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.refineV
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Decoder
import lucuma.core.model.NonNegDuration
import lucuma.core.model.given
import lucuma.refined.*
import monocle.Focus
import monocle.Lens
import monocle.Optional
import monocle.POptional
import monocle.std.either.*
import monocle.std.option
import org.typelevel.cats.time.instances.zoneddatetime.*

import java.time.Duration
import java.time.ZonedDateTime

case class TimingWindowRepeatPeriod(
  period:          NonNegDuration,
  repeatFrequency: Option[PosInt] // Times repetition or forever
) derives Eq {
  def toForever: TimingWindowRepeatPeriod = copy(repeatFrequency = None)
  def toNTimes(n: PosInt): TimingWindowRepeatPeriod = copy(repeatFrequency = n.some)
}

object TimingWindowRepeatPeriod:
  val period: Lens[TimingWindowRepeatPeriod, NonNegDuration] =
    Focus[TimingWindowRepeatPeriod](_.period)

  val repeatFrequency: Lens[TimingWindowRepeatPeriod, Option[PosInt]] =
    Focus[TimingWindowRepeatPeriod](_.repeatFrequency)

case class TimingWindowRepeat(
  remainOpenFor: NonNegDuration,
  repeatPeriod:  Option[TimingWindowRepeatPeriod]
) derives Eq {}

object TimingWindowRepeat:
  def remainOpenFor(openFor: NonNegDuration) = TimingWindowRepeat(openFor, None)

  val remainOpenFor: Lens[TimingWindowRepeat, NonNegDuration] =
    Focus[TimingWindowRepeat](_.remainOpenFor)

  val repeatPeriod: Lens[TimingWindowRepeat, Option[TimingWindowRepeatPeriod]] =
    Focus[TimingWindowRepeat](_.repeatPeriod)

case class TimingWindow private (
  id:         Int,
  startsOn:   ZonedDateTime,
  repetition: Option[Either[ZonedDateTime, TimingWindowRepeat]]
) derives Eq {
  def toCloseOn(closeOn: ZonedDateTime): TimingWindow =
    copy(repetition = closeOn.asLeft.some)

  def toForever: TimingWindow =
    copy(repetition = None)

  def toRemainOpen(duration: NonNegDuration): TimingWindow =
    copy(repetition = TimingWindowRepeat(duration, None).asRight.some)

  def toRepeatPeriod(period: NonNegDuration): TimingWindow =
    copy(repetition =
      repetition.map(_.map(_.copy(repeatPeriod = TimingWindowRepeatPeriod(period, None).some)))
    )

  def noRepeatPeriod: TimingWindow =
    copy(repetition = repetition.map(_.map(_.copy(repeatPeriod = none))))

  def toRepeatPeriodForever: TimingWindow =
    copy(repetition =
      repetition.map(
        _.map(u => u.copy(repeatPeriod = u.repeatPeriod.map(_.copy(repeatFrequency = None))))
      )
    )

  def toRepeatPeriodNTimes(times: PosInt): TimingWindow =
    copy(repetition =
      repetition.map(
        _.map { u =>
          u.copy(remainOpenFor = u.remainOpenFor,
                 repeatPeriod = u.repeatPeriod.map(_.copy(repeatFrequency = times.some))
          )
        }
      )
    )

  def openForever: Boolean = repetition.isEmpty

  def closeOn: Boolean = repetition.exists(_.isLeft)

  def remainOpenFor: Boolean = repetition.exists(_.isRight)

  def repeatPeriod: Boolean = repetition.exists(_.toOption.exists(_.repeatPeriod.isDefined))

  def repeatForever: Boolean =
    repetition.exists(_.toOption.exists(_.repeatPeriod.exists(_.repeatFrequency.isEmpty)))
}

object TimingWindow:
  val startsOn: Lens[TimingWindow, ZonedDateTime] = Focus[TimingWindow](_.startsOn)

  val closeOn: Optional[TimingWindow, ZonedDateTime] =
    Focus[TimingWindow](_.repetition).some.andThen(stdLeft)

  val remainOpenFor: Optional[TimingWindow, NonNegDuration] =
    Focus[TimingWindow](_.repetition).some
      .andThen(stdRight)
      .andThen(TimingWindowRepeat.remainOpenFor)

  val repeatPeriod: Optional[TimingWindow, NonNegDuration] =
    Focus[TimingWindow](_.repetition).some
      .andThen(stdRight)
      .andThen(TimingWindowRepeat.repeatPeriod)
      .andThen(option.some)
      .andThen(TimingWindowRepeatPeriod.period)

  val repeatFrequency: Optional[TimingWindow, PosInt] =
    Focus[TimingWindow](_.repetition).some
      .andThen(stdRight)
      .andThen(TimingWindowRepeat.repeatPeriod)
      .andThen(option.some)
      .andThen(TimingWindowRepeatPeriod.repeatFrequency)
      .andThen(option.some)

  def forever(id: Int, startsOn: ZonedDateTime): TimingWindow =
    new TimingWindow(id, startsOn, None)

  def closeOn(id: Int, startsOn: ZonedDateTime, closeOn: ZonedDateTime): TimingWindow =
    new TimingWindow(id, startsOn, repetition = closeOn.asLeft.some)

  def remainOpenFor(id: Int, startsOn: ZonedDateTime, remainOpen: NonNegDuration): TimingWindow =
    new TimingWindow(id, startsOn, repetition = TimingWindowRepeat(remainOpen, None).asRight.some)

  def remainOpenForWithPeriod(
    id:         Int,
    startsOn:   ZonedDateTime,
    remainOpen: NonNegDuration,
    period:     NonNegDuration
  ): TimingWindow =
    new TimingWindow(
      id,
      startsOn,
      repetition =
        TimingWindowRepeat(remainOpen, Some(TimingWindowRepeatPeriod(period, None))).asRight.some
    )

  def remainOpenForTimes(
    id:         Int,
    startsOn:   ZonedDateTime,
    remainOpen: NonNegDuration,
    period:     NonNegDuration,
    times:      PosInt
  ): TimingWindow =
    new TimingWindow(
      id,
      startsOn,
      repetition = TimingWindowRepeat(remainOpen,
                                      Some(TimingWindowRepeatPeriod(period, times.some))
      ).asRight.some
    )

// This is a model for the table in hasura which is very basic
// It will be superseeded on the real api
case class TimingWindowEntry(
  id:            Int,
  startsOn:      ZonedDateTime,
  forever:       Boolean,
  repeatPeriod:  Option[Int] = None,
  repeatForever: Option[Boolean] = None,
  repeatTimes:   Option[Int] = None,
  remainOpenFor: Option[Int] = None, // Seconds
  closeOn:       Option[ZonedDateTime] = None
)

object TimingWindowEntry:
  def fromTimingWindow(tw: TimingWindow): TimingWindowEntry =
    tw match
      case TimingWindow(
            id,
            startsOn,
            Some(
              Right(
                TimingWindowRepeat(remainOpen, Some(TimingWindowRepeatPeriod(period, Some(times))))
              )
            )
          ) =>
        TimingWindowEntry(id,
                          startsOn,
                          false,
                          repeatPeriod = period.value.getSeconds.toInt.some,
                          remainOpenFor = remainOpen.value.getSeconds.toInt.some,
                          repeatTimes = times.value.some
        )
      case TimingWindow(
            id,
            startsOn,
            Some(
              Right(TimingWindowRepeat(remainOpen, Some(TimingWindowRepeatPeriod(period, None))))
            )
          ) =>
        TimingWindowEntry(id,
                          startsOn,
                          false,
                          remainOpenFor = remainOpen.value.getSeconds.toInt.some,
                          repeatForever = true.some,
                          repeatPeriod = period.value.getSeconds.toInt.some
        )
      case TimingWindow(id, startsOn, Some(Right(TimingWindowRepeat(remainOpen, None)))) =>
        TimingWindowEntry(id,
                          startsOn,
                          false,
                          remainOpenFor = remainOpen.value.getSeconds.toInt.some
        )
      case TimingWindow(id, startsOn, Some(Left(repetition)))                            =>
        TimingWindowEntry(id, startsOn, false, closeOn = repetition.some)
      case TimingWindow(id, startsOn, None)                                              =>
        TimingWindowEntry(id, startsOn, true)

  def toTimingWindow(tw: TimingWindowEntry): TimingWindow =
    tw match
      case TimingWindowEntry(id,
                             startsOn,
                             false,
                             Some(repeatPeriod),
                             _,
                             Some(times),
                             Some(remainOpenFor),
                             _
          ) =>
        TimingWindow.remainOpenForTimes(
          id,
          startsOn,
          NonNegDuration.unsafeFrom(Duration.ofSeconds(remainOpenFor)),
          NonNegDuration.unsafeFrom(Duration.ofSeconds(repeatPeriod)),
          refineV[Positive](times).getOrElse(1.refined)
        )
      case TimingWindowEntry(id,
                             startsOn,
                             false,
                             Some(repeatPeriod),
                             _,
                             _,
                             Some(remainOpenFor),
                             _
          ) =>
        TimingWindow.remainOpenForWithPeriod(
          id,
          startsOn,
          NonNegDuration.unsafeFrom(Duration.ofSeconds(remainOpenFor)),
          NonNegDuration.unsafeFrom(Duration.ofSeconds(repeatPeriod))
        )
      case TimingWindowEntry(id, startsOn, false, _, _, _, Some(remainOpenFor), _) =>
        TimingWindow.remainOpenFor(id,
                                   startsOn,
                                   NonNegDuration.unsafeFrom(Duration.ofSeconds(remainOpenFor))
        )
      case TimingWindowEntry(id, startsOn, false, _, _, _, _, Some(closeOn))       =>
        TimingWindow.closeOn(id, startsOn, closeOn)
      case TimingWindowEntry(id, startsOn, true, _, _, _, _, _)                    =>
        TimingWindow.forever(id, startsOn)
      case _                                                                       =>
        sys.error("Case not covered on the db model")
