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
import lucuma.core.math.BoundedInterval
import lucuma.core.model.given
import lucuma.core.util.TimeSpan
import lucuma.refined.*
import monocle.Focus
import monocle.Lens
import monocle.Optional
import monocle.POptional
import monocle.std.either.*
import monocle.std.option
import org.typelevel.cats.time.given
import spire.math.Interval
import spire.math.extras.interval.IntervalSeq

import java.time.Duration
import java.time.Instant
import java.time.ZonedDateTime

case class TimingWindowRepeatPeriod(
  period:          TimeSpan,
  repeatFrequency: Option[PosInt] // Times repetition or forever
) derives Eq {
  def toForever: TimingWindowRepeatPeriod = copy(repeatFrequency = None)
  def toNTimes(n: PosInt): TimingWindowRepeatPeriod = copy(repeatFrequency = n.some)
}

object TimingWindowRepeatPeriod:
  val period: Lens[TimingWindowRepeatPeriod, TimeSpan] =
    Focus[TimingWindowRepeatPeriod](_.period)

  val repeatFrequency: Lens[TimingWindowRepeatPeriod, Option[PosInt]] =
    Focus[TimingWindowRepeatPeriod](_.repeatFrequency)

case class TimingWindowRepeat(
  remainOpenFor: TimeSpan,
  repeatPeriod:  Option[TimingWindowRepeatPeriod]
) derives Eq {}

object TimingWindowRepeat:
  def remainOpenFor(openFor: TimeSpan) = TimingWindowRepeat(openFor, None)

  val remainOpenFor: Lens[TimingWindowRepeat, TimeSpan] =
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

  def toRemainOpen(duration: TimeSpan): TimingWindow =
    copy(repetition = TimingWindowRepeat(duration, None).asRight.some)

  def toRepeatPeriod(period: TimeSpan): TimingWindow =
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

  def toIntervalSeq(within: BoundedInterval[Instant]): IntervalSeq[Instant] = {
    // Builds a bunch of single-interval `IntervalSeq`s, for each of the `starts` provided, each lasting `duration`.
    // Returns the union of all of them.
    def intervalsForStarts(starts: List[Instant], duration: Duration): IntervalSeq[Instant] =
      starts
        .map(start => IntervalSeq(Interval(start, start.plus(duration))))
        .foldLeft(IntervalSeq.empty[Instant])(_ | _)

    val startInstant = startsOn.toInstant
    val intervals    = repetition match
      case None                                                 =>
        IntervalSeq.atOrAbove(startInstant)
      case Some(Left(endsOn))                                   =>
        IntervalSeq(Interval(startInstant, endsOn.toInstant))
      case Some(Right(TimingWindowRepeat(remainOpenFor, None))) =>
        IntervalSeq(Interval(startInstant, startInstant.plus(remainOpenFor.toDuration)))
      case Some(
            Right(
              TimingWindowRepeat(remainOpenFor, Some(TimingWindowRepeatPeriod(period, Some(times))))
            )
          ) =>
        intervalsForStarts(
          List.unfold((0, startInstant))(
            _.some
              .filter(_._1 <= times.value)
              .map((iter, start) => (start, (iter + 1, start.plus(period.toDuration))))
          ),
          remainOpenFor.toDuration
        )
      case Some(
            Right(TimingWindowRepeat(remainOpenFor, Some(TimingWindowRepeatPeriod(period, None))))
          ) =>
        intervalsForStarts(
          List.unfold(startInstant)(
            _.some.filter(_ < within.upper).map(start => (start, start.plus(period.toDuration)))
          ),
          remainOpenFor.toDuration
        )

    intervals & IntervalSeq(within)
  }
}

object TimingWindow:
  val startsOn: Lens[TimingWindow, ZonedDateTime] = Focus[TimingWindow](_.startsOn)

  val closeOn: Optional[TimingWindow, ZonedDateTime] =
    Focus[TimingWindow](_.repetition).some.andThen(stdLeft)

  val remainOpenFor: Optional[TimingWindow, TimeSpan] =
    Focus[TimingWindow](_.repetition).some
      .andThen(stdRight)
      .andThen(TimingWindowRepeat.remainOpenFor)

  val repeatPeriod: Optional[TimingWindow, TimeSpan] =
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

  def remainOpenFor(id: Int, startsOn: ZonedDateTime, remainOpen: TimeSpan): TimingWindow =
    new TimingWindow(id, startsOn, repetition = TimingWindowRepeat(remainOpen, None).asRight.some)

  def remainOpenForWithPeriod(
    id:         Int,
    startsOn:   ZonedDateTime,
    remainOpen: TimeSpan,
    period:     TimeSpan
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
    remainOpen: TimeSpan,
    period:     TimeSpan,
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
                          repeatPeriod = period.toSeconds.intValue.some,
                          remainOpenFor = remainOpen.toSeconds.intValue.some,
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
                          remainOpenFor = remainOpen.toSeconds.intValue.some,
                          repeatForever = true.some,
                          repeatPeriod = period.toSeconds.intValue.some
        )
      case TimingWindow(id, startsOn, Some(Right(TimingWindowRepeat(remainOpen, None)))) =>
        TimingWindowEntry(id, startsOn, false, remainOpenFor = remainOpen.toSeconds.intValue.some)
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
          TimeSpan.unsafeFromDuration(Duration.ofSeconds(remainOpenFor)),
          TimeSpan.unsafeFromDuration(Duration.ofSeconds(repeatPeriod)),
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
          TimeSpan.unsafeFromDuration(Duration.ofSeconds(remainOpenFor)),
          TimeSpan.unsafeFromDuration(Duration.ofSeconds(repeatPeriod))
        )
      case TimingWindowEntry(id, startsOn, false, _, _, _, Some(remainOpenFor), _) =>
        TimingWindow.remainOpenFor(
          id,
          startsOn,
          TimeSpan.unsafeFromDuration(Duration.ofSeconds(remainOpenFor))
        )
      case TimingWindowEntry(id, startsOn, false, _, _, _, _, Some(closeOn))       =>
        TimingWindow.closeOn(id, startsOn, closeOn)
      case TimingWindowEntry(id, startsOn, true, _, _, _, _, _)                    =>
        TimingWindow.forever(id, startsOn)
      case _                                                                       =>
        sys.error("Case not covered on the db model")
