// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import cats.Applicative
import cats.Endo
import cats.Semigroup
import cats.effect.*
import cats.effect.Temporal
import cats.effect.syntax.all.*
import cats.syntax.all.*
import clue.data.*
import clue.data.syntax.*
import eu.timepit.refined.*
import eu.timepit.refined.types.numeric.NonNegLong
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import explore.BuildInfo
import explore.components.ui.ExploreStyles
import fs2.Chunk
import fs2.Pipe
import fs2.Pull
import fs2.Stream
import fs2.concurrent.Channel
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ExecutionEnvironment
import lucuma.core.util.NewType
import lucuma.core.util.TimeSpan
import lucuma.react.datepicker.*
import lucuma.react.fa.IconSize
import lucuma.react.primereact.Button
import lucuma.react.primereact.Message
import lucuma.react.primereact.MessageItem
import lucuma.react.primereact.ToastRef
import lucuma.ui.LucumaIcons
import lucuma.ui.LucumaStyles
import lucuma.ui.components.TimeSpanView
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.versionDateFormatter
import lucuma.ui.utils.versionDateTimeFormatter
import org.http4s.Uri
import org.scalajs.dom

import java.time.Instant
import java.time.ZoneOffset
import java.time.ZonedDateTime
import scala.concurrent.duration.FiniteDuration
import scala.scalajs.js

val canvasWidth  = VdomAttr("width")
val canvasHeight = VdomAttr("height")
val dataAbbrv    = VdomAttr("data-abbrv")
val filter       = VdomAttr("filter")

def toggleReusabilityOverlay[F[_]: Sync](): F[Unit] =
  Sync[F]
    .delay(
      dom.document.body.classList.toggle(ExploreStyles.HideReusability.htmlClass)
    )
    .void

val gitHash = BuildInfo.gitHeadCommit

def version(environment: ExecutionEnvironment): NonEmptyString = {
  val instant = Instant.ofEpochMilli(BuildInfo.buildDateTime)
  NonEmptyString.unsafeFrom(
    (environment match
      case ExecutionEnvironment.Development =>
        versionDateTimeFormatter.format(instant)
      case _                                =>
        versionDateFormatter.format(instant) +
          "-" + gitHash.map(_.take(7)).getOrElse("NONE")
    )
      + environment.suffix
        .map(suffix => s"-$suffix")
        .orEmpty
  )
}

inline def showCount(count: Int, unit: String, plural: String): String =
  if (count == 1) s"$count $unit"
  else s"$count $plural"

inline def showCount(count: Int, unit: String): String =
  showCount(count, unit, unit + "s")

extension (uri: Uri)
  def addPath(p: Uri.Path): Uri =
    uri.withPath(uri.path.concat(p))

def forceAssign[T, S](mod: Endo[Input[S]] => Endo[T])(base: S): Endo[S] => Endo[T] =
  modS =>
    mod:
      case Assign(edit) => modS(edit).assign
      case _            => modS(base).assign

extension [F[_]: Sync: ToastCtx](f: F[Unit])
  def withToast(text: String, severity: Message.Severity = Message.Severity.Info): F[Unit] =
    f <* ToastCtx[F].showToast(text, severity)

extension (toastRef: ToastRef)
  def upgradePrompt(text: VdomNode, callback: Callback): Callback =
    toastRef.show(
      MessageItem(
        content = <.div(
          ExploreStyles.ExplorePromptToast,
          <.span(
            LucumaIcons.CircleInfo.withSize(IconSize.LG),
            text
          ),
          Button(size = Button.Size.Small, onClick = toastRef.clear() *> callback)("Upgrade ...")
        ),
        clazz = LucumaStyles.Toast,
        sticky = true
      )
    )

// TODO Move these to react-datetime
extension (instant:  Instant)
  // DatePicker only works in local timezone, so we trick it by adding the timezone offset.
  // See https://github.com/Hacker0x01/react-datepicker/issues/1787
  def toDatePickerJsDate: js.Date                                 =
    val zdt: ZonedDateTime = ZonedDateTime.ofInstant(instant, ZoneOffset.UTC)
    // init removes the Z timezone
    new js.Date(js.Date.parse(zdt.toString.init))

extension (zdt: ZonedDateTime)
  // DatePicker only works in local timezone, so we trick it by adding the timezone offset.
  // See https://github.com/Hacker0x01/react-datepicker/issues/1787
  def toDatePickerJsDate: js.Date =
    zdt.toInstant.toDatePickerJsDate

// DatePicker only works in local timezone, so we trick it by adding the timezone offset.
// See https://github.com/Hacker0x01/react-datepicker/issues/1787
private def fromDatePickerJsDate(jsDate: js.Date): Instant =
  Instant.ofEpochMilli((jsDate.getTime() - jsDate.getTimezoneOffset() * 60000).toLong)

extension [A](value: js.UndefOr[DateOrRange])
  def fromDatePickerToInstantEitherOpt(using
    A <:< js.Date
  ): Option[Either[(Instant, Instant), Instant]] =
    value.toEitherOpt.map { (e: Either[(js.Date, js.Date), js.Date]) =>
      e match {
        case Left((d1, d2)) =>
          Left((fromDatePickerJsDate(d1), fromDatePickerJsDate(d2)))
        case Right(d)       =>
          Right(fromDatePickerJsDate(d))
      }
    }.widen

  def fromDatePickerToInstantOpt(using ev: A <:< js.Date): Option[Instant] =
    fromDatePickerToInstantEitherOpt.flatMap(_.toOption)

  def fromDatePickerToZDTOpt(using ev: A <:< js.Date): Option[ZonedDateTime] =
    fromDatePickerToInstantOpt.map(i => ZonedDateTime.ofInstant(i, ZoneOffset.UTC))

extension (bytes: NonNegLong)
  // modified from: https://gist.github.com/BlinkoWang/38b706cb24fa91b1d761
  def toHumanReadableByteCount: String = {
    val base = 1000
    if (bytes.value < base) s"$bytes B"
    else {
      val exp   = (Math.log(bytes.value.toDouble) / Math.log(base)).toInt
      val pre   = "kMGTPE".charAt(exp - 1)
      val value = bytes.value / Math.pow(base, exp)
      f"$value%.2f ${pre}B"
    }
  }

object IsExpanded extends NewType[Boolean]
type IsExpanded = IsExpanded.Type

extension [F[_], O](s: Stream[F, O])
  /**
   * Combine items within a given duration using Semigroup.
   *
   * When an item is received in the stream, a timer is started. Within that time, items are
   * combined until the timer expires, at which point the combined item is emitted.
   *
   * If no items are received within the given duration, no item is emitted.
   */
  def reduceSemigroupWithin(d: FiniteDuration)(using
    F: Temporal[F],
    S: Semigroup[O]
  ): Stream[F, O] =
    reduceWithin(d, S.combine)

  /**
   * Combine items within a given duration using the given function.
   *
   * When an item is received in the stream, a timer is started. Within that time, items are
   * combined until the timer expires, at which point the combined item is emitted.
   *
   * If no items are received within the given duration, no item is emitted.
   *
   * This might seem similar to `groupWithin`. The subtle difference is that in-between durations
   * `groupWithin` enters a "timed out" state, where the next element will immediately be emitted,
   * and only elements _after_ that will be grouped. This is not the case for `reduceWithin`, which
   * will always wait for the timeout to expire before emitting the combined element.
   */
  def reduceWithin(
    d: FiniteDuration,
    f: (O, O) => O
  )(using F: Temporal[F]): Stream[F, O] =
    Stream.force {
      F.ref[Option[O]](None).map { ref =>

        val sendLatest: Pull[F, O, Unit] =
          // 'clear' the ref
          Pull.eval(ref.getAndSet(None)).flatMap {
            // Don't send empty elements
            case None    => Pull.done
            // Send a combined chunk
            case Some(c) => Pull.output1(c)
          }

        // Start a 'timed' pull. This will send a timeout cons after the given timeout, at which point we send the collected chunk
        s.pull.timed { timedPull =>

          def combine(x: Option[O], y: Option[O]) = (x, y) match
            case (None, None)       => None
            case (Some(a), None)    => Some(a)
            case (None, Some(b))    => Some(b)
            case (Some(a), Some(b)) => Some(f(a, b))

          def addToSend(oChunk: Chunk[O]): Pull[F, Nothing, Unit] =
            Pull
              .eval(ref.getAndUpdate(combine(_, oChunk.reduceLeftOption(f))))
              .flatMap {
                // This is the first new element since the last emit (or first in the stream), so start the timer
                case None => timedPull.timeout(d)
                // Timed is started already, so do nothing
                case _    => Pull.done
              }

          def go(timedPull: Pull.Timed[F, O]): Pull[F, O, Unit] =
            timedPull.uncons.flatMap {
              // Combine chunk to send when the timeout expires
              case Some((Right(chunk), next)) => addToSend(chunk) >> go(next)
              // Timeout has expired, send the latest element
              case Some((Left(_), next))      => sendLatest >> go(next)
              // Stream is done, send the latest element
              case None                       => sendLatest
            }
          go(timedPull)

        }.stream
      }
    }

/**
 * Produces a `Pipe` that evaluates an effect for each element in the input stream and emits its
 * result, canceling previous effects that are still running with the same `key`.
 */
def keyedSwitchEvalMap[F[_]: Concurrent, I, O, K](
  key: I => K,
  f:   I => F[O]
): Pipe[F, I, O] = {
  // The logic uses an internal `Channel` to where completed effects emit their values (together with their corresponding key).
  // It then uses a `Pull` that processes one by one the elements of a stream that merges the the input and result streams.
  // The input stream is on the `Left`, `noneTerminate`d so we can tell when it ends. New elements here trigger the effect and cancel running one for the same key.
  // The result stream is on the `Right`, with each element together with its associated key. New elements here are emitted in the `Pull` output.
  // We also keep track of the running effects and whether the input stream has ended.
  def go(
    stream: Stream[F, Either[Option[I], (K, O)]],
    fibers: Map[K, Fiber[F, Throwable, Unit]],
    ended:  Boolean,
    emit:   ((K, O)) => F[Unit]
  ): Pull[F, O, Unit] =
    stream.pull.uncons1.flatMap:
      // Element arrives on input stream. Run the effect and store the fiber. Cancel previous effect for the same key, if any.
      case Some(Left(Some(i)), tail) =>
        val k: K = key(i)

        def run(preF: F[Unit]): Pull[F, O, Unit] =
          // Before running the effect, cancel the previous one for the same key, if any.
          // After running the effect, emit its value to the result stream. This is processed in the next `case` entry.
          val finalF = preF >> f(i) >>= (o => emit(k -> o))
          Pull
            .eval(finalF.start)
            .flatMap: fiber =>
              go(tail, fibers.updated(k, fiber), ended, emit)

        fibers.get(k) match
          case Some(fiber) => run(fiber.cancel)
          case None        => run(Applicative[F].unit)
      // An effect completed! Output it. If input stream ended and there no more running fibers, we're done.
      case Some(Right((k, o)), tail) =>
        Pull.output1(o) >> {
          val newFibers = fibers - k
          if (ended && newFibers.isEmpty) Pull.done
          else go(tail, newFibers, ended, emit)
        }
      // Input stream ended! Just take note and wait for all fibers to complete, or end if there are no fibers.
      case Some(Left(None), tail)    =>
        if (fibers.isEmpty) Pull.done
        else go(tail, fibers, true, emit)
      // Will never happen. Right stream will not end on its own.
      case None                      => Pull.done

  in =>
    Stream
      .eval(Channel.unbounded[F, (K, O)])
      .flatMap: out =>
        go(in.noneTerminate.either(out.stream), Map.empty, false, out.send(_).void).stream
          .onFinalize(out.close.void)
}

def timeDisplay(name: String, time: TimeSpan, sep: String = ": ") =
  <.span(<.span(ExploreStyles.SequenceTileTitleItem)(name, sep), TimeSpanView(time))

extension (s: NonNegShort)
  def +|(i: Int): NonNegShort =
    NonNegShort.unsafeFrom:
      val int = s.value + i
      if int > Short.MaxValue then Short.MaxValue
      else int.toShort

  def |-(i: Int): NonNegShort =
    NonNegShort.unsafeFrom:
      val int = s.value - i
      if int < 0 then 0
      else int.toShort
