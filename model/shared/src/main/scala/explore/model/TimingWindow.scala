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
import lucuma.core.math.BoundedInterval
import lucuma.core.model.given
import lucuma.core.util.Display
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

enum TimingWindowType derives Eq:
  case Include, Exclude

  def isInclude: Boolean = this == Include
  def isExclude: Boolean = !isInclude

object TimingWindowType:
  def fromInclude(include: Boolean): TimingWindowType =
    if (include) Include else Exclude

  given Display[TimingWindowType] = Display.byShortName(_ match
    case Include => "Include"
    case Exclude => "Exclude"
  )

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
  finiteSpan:   TimeSpan,
  repeatPeriod: Option[TimingWindowRepeatPeriod]
) derives Eq {}

object TimingWindowRepeat:
  def finiteSpan(value: TimeSpan) = TimingWindowRepeat(value, None)

  val finiteSpan: Lens[TimingWindowRepeat, TimeSpan] =
    Focus[TimingWindowRepeat](_.finiteSpan)

  val repeatPeriod: Lens[TimingWindowRepeat, Option[TimingWindowRepeatPeriod]] =
    Focus[TimingWindowRepeat](_.repeatPeriod)

case class TimingWindow private (
  id:         Int,
  windowType: TimingWindowType,
  from:       ZonedDateTime,
  repetition: Option[Either[ZonedDateTime, TimingWindowRepeat]]
) derives Eq {
  def through(end: ZonedDateTime): TimingWindow =
    copy(repetition = end.asLeft.some)

  def toForever: TimingWindow =
    copy(repetition = None)

  def toFiniteSpan(duration: TimeSpan): TimingWindow =
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
          u.copy(
            finiteSpan = u.finiteSpan,
            repeatPeriod = u.repeatPeriod.map(_.copy(repeatFrequency = times.some))
          )
        }
      )
    )

  def forever: Boolean = repetition.isEmpty

  def through: Boolean = repetition.exists(_.isLeft)

  def finiteSpan: Boolean = repetition.exists(_.isRight)

  def repeatPeriod: Boolean = repetition.exists(_.toOption.exists(_.repeatPeriod.isDefined))

  def repeatForever: Boolean =
    repetition.exists(_.toOption.exists(_.repeatPeriod.exists(_.repeatFrequency.isEmpty)))

  export windowType.{isExclude, isInclude}

  def toIntervalSeq(within: BoundedInterval[Instant]): IntervalSeq[Instant] = {
    // Builds a bunch of single-interval `IntervalSeq`s, for each of the `starts` provided, each lasting `duration`.
    // Returns the union of all of them.
    def intervalsForStarts(starts: List[Instant], duration: Duration): IntervalSeq[Instant] =
      starts
        .map(start => IntervalSeq(Interval(start, start.plus(duration))))
        .foldLeft(IntervalSeq.empty[Instant])(_ | _)

    val windowStart = from.toInstant()

    // find the start of a repeat window nearest to the start of the chart
    def windowStartForPeriod(period: TimeSpan) =
      windowStart
        .plusMillis(
          period.toDuration
            .multipliedBy(
              Duration.between(windowStart, within.lower).toMillis() / period.toDuration
                .toMillis()
            )
            .toMillis()
        )
        .max(windowStart) // window start could be set after the chart range

    val intervals = repetition match
      case None                                              =>
        IntervalSeq.atOrAbove(windowStart)
      case Some(Left(endsOn))                                =>
        IntervalSeq(Interval(windowStart, endsOn.toInstant))
      case Some(Right(TimingWindowRepeat(finiteSpan, None))) =>
        IntervalSeq(Interval(windowStart, windowStart.plus(finiteSpan.toDuration)))
      // Repeat period n times
      case Some(
            Right(
              TimingWindowRepeat(
                finiteSpan,
                Some(TimingWindowRepeatPeriod(period, Some(times)))
              )
            )
          ) =>
        val nearestStart = windowStartForPeriod(period)
        intervalsForStarts(
          List.unfold((0, nearestStart))(
            _.some
              .filter(_._1 <= times.value)
              .filter(_._2 <= within.upper)
              .map((iter, start) => (start, (iter + 1, start.plus(period.toDuration))))
          ),
          finiteSpan.toDuration
        )
      // Repeat period for ever
      case Some(
            Right(TimingWindowRepeat(finiteSpan, Some(TimingWindowRepeatPeriod(period, None))))
          ) =>
        val nearestStart = windowStartForPeriod(period)
        intervalsForStarts(
          List.unfold(nearestStart)(
            _.some
              .filter(i => i <= within.upper)
              .map(start => (start, start.plus(period.toDuration)))
          ),
          finiteSpan.toDuration
        )

    intervals & IntervalSeq(within)
  }
}

object TimingWindow:
  val windowType: Lens[TimingWindow, TimingWindowType] = Focus[TimingWindow](_.windowType)

  val from: Lens[TimingWindow, ZonedDateTime] = Focus[TimingWindow](_.from)

  val through: Optional[TimingWindow, ZonedDateTime] =
    Focus[TimingWindow](_.repetition).some.andThen(stdLeft)

  val finiteSpan: Optional[TimingWindow, TimeSpan] =
    Focus[TimingWindow](_.repetition).some
      .andThen(stdRight)
      .andThen(TimingWindowRepeat.finiteSpan)

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

  def forever(id: Int, windowType: TimingWindowType, startsOn: ZonedDateTime): TimingWindow =
    new TimingWindow(id, windowType, startsOn, None)

  def through(
    id:         Int,
    windowType: TimingWindowType,
    startsOn:   ZonedDateTime,
    through:    ZonedDateTime
  ): TimingWindow =
    new TimingWindow(id, windowType, startsOn, repetition = through.asLeft.some)

  def remainOpenFor(
    id:         Int,
    windowType: TimingWindowType,
    startsOn:   ZonedDateTime,
    remainOpen: TimeSpan
  ): TimingWindow =
    new TimingWindow(
      id,
      windowType,
      startsOn,
      repetition = TimingWindowRepeat(remainOpen, None).asRight.some
    )

  def remainOpenForWithPeriod(
    id:         Int,
    windowType: TimingWindowType,
    startsOn:   ZonedDateTime,
    remainOpen: TimeSpan,
    period:     TimeSpan
  ): TimingWindow =
    new TimingWindow(
      id,
      windowType,
      startsOn,
      repetition =
        TimingWindowRepeat(remainOpen, Some(TimingWindowRepeatPeriod(period, None))).asRight.some
    )

  def remainOpenForTimes(
    id:         Int,
    windowType: TimingWindowType,
    startsOn:   ZonedDateTime,
    remainOpen: TimeSpan,
    period:     TimeSpan,
    times:      PosInt
  ): TimingWindow =
    new TimingWindow(
      id,
      windowType,
      startsOn,
      repetition = TimingWindowRepeat(remainOpen,
                                      Some(TimingWindowRepeatPeriod(period, times.some))
      ).asRight.some
    )

// This is a model for the table in hasura which is very basic
// It will be superseeded on the real api
case class TimingWindowEntry(
  id:            Int,
  include:       Boolean,
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
            windowType,
            startsOn,
            Some(
              Right(
                TimingWindowRepeat(remainOpen, Some(TimingWindowRepeatPeriod(period, Some(times))))
              )
            )
          ) =>
        TimingWindowEntry(
          id,
          windowType.isInclude,
          startsOn,
          false,
          repeatPeriod = period.toSeconds.intValue.some,
          remainOpenFor = remainOpen.toSeconds.intValue.some,
          repeatTimes = times.value.some
        )
      case TimingWindow(
            id,
            windowType,
            startsOn,
            Some(
              Right(TimingWindowRepeat(remainOpen, Some(TimingWindowRepeatPeriod(period, None))))
            )
          ) =>
        TimingWindowEntry(
          id,
          windowType.isInclude,
          startsOn,
          false,
          remainOpenFor = remainOpen.toSeconds.intValue.some,
          repeatForever = true.some,
          repeatPeriod = period.toSeconds.intValue.some
        )
      case TimingWindow(
            id,
            windowType,
            startsOn,
            Some(Right(TimingWindowRepeat(remainOpen, None)))
          ) =>
        TimingWindowEntry(
          id,
          windowType.isInclude,
          startsOn,
          false,
          remainOpenFor = remainOpen.toSeconds.intValue.some
        )
      case TimingWindow(id, windowType, startsOn, Some(Left(repetition))) =>
        TimingWindowEntry(id, windowType.isInclude, startsOn, false, closeOn = repetition.some)
      case TimingWindow(id, windowType, startsOn, None)                   =>
        TimingWindowEntry(id, windowType.isInclude, startsOn, true)

  def toTimingWindow(tw: TimingWindowEntry): TimingWindow =
    tw match
      case TimingWindowEntry(
            id,
            include,
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
          TimingWindowType.fromInclude(include),
          startsOn,
          TimeSpan.unsafeFromDuration(Duration.ofSeconds(remainOpenFor)),
          TimeSpan.unsafeFromDuration(Duration.ofSeconds(repeatPeriod)),
          refineV[Positive](times).getOrElse(1.refined)
        )
      case TimingWindowEntry(
            id,
            include,
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
          TimingWindowType.fromInclude(include),
          startsOn,
          TimeSpan.unsafeFromDuration(Duration.ofSeconds(remainOpenFor)),
          TimeSpan.unsafeFromDuration(Duration.ofSeconds(repeatPeriod))
        )
      case TimingWindowEntry(id, include, startsOn, false, _, _, _, Some(remainOpenFor), _) =>
        TimingWindow.remainOpenFor(
          id,
          TimingWindowType.fromInclude(include),
          startsOn,
          TimeSpan.unsafeFromDuration(Duration.ofSeconds(remainOpenFor))
        )
      case TimingWindowEntry(id, include, startsOn, false, _, _, _, _, Some(closeOn))       =>
        TimingWindow.through(id, TimingWindowType.fromInclude(include), startsOn, closeOn)
      case TimingWindowEntry(id, include, startsOn, true, _, _, _, _, _)                    =>
        TimingWindow.forever(id, TimingWindowType.fromInclude(include), startsOn)
      case _                                                                                =>
        sys.error("Case not covered on the db model")
