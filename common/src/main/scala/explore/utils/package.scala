// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import cats.Endo
import cats.Semigroup
import cats.effect.Temporal
import cats.effect.*
import cats.syntax.all.*
import clue.data.*
import clue.data.syntax.*
import eu.timepit.refined.*
import eu.timepit.refined.types.numeric.NonNegLong
import eu.timepit.refined.types.string.NonEmptyString
import explore.BuildInfo
import explore.Icons
import explore.components.ui.ExploreStyles
import fs2.Chunk
import fs2.Pull
import fs2.Stream
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.NewType
import lucuma.react.datepicker.*
import lucuma.react.fa.IconSize
import lucuma.react.primereact.Button
import lucuma.react.primereact.Message
import lucuma.react.primereact.MessageItem
import lucuma.react.primereact.ToastRef
import lucuma.ui.enums.ExecutionEnvironment
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

// TODO Move these to lucuma-ui
extension (toastRef: ToastRef)
  def show(
    text:     String,
    severity: Message.Severity = Message.Severity.Info,
    sticky:   Boolean = false
  ): Callback =
    toastRef.show(
      MessageItem(
        content = <.span(Icons.InfoLight.withSize(IconSize.LG), text),
        severity = severity,
        sticky = sticky,
        clazz = ExploreStyles.ExploreToast
      )
    )

  def upgradePrompt(text: VdomNode, callback: Callback): Callback =
    toastRef.show(
      MessageItem(
        content = <.div(
          ExploreStyles.ExplorePromptToast,
          <.span(
            Icons.InfoLight.withSize(IconSize.LG),
            text
          ),
          Button(size = Button.Size.Small, onClick = toastRef.clear() *> callback)("Upgrade ...")
        ),
        clazz = ExploreStyles.ExploreToast,
        sticky = true
      )
    )

extension [F[_]: Sync: ToastCtx](f: F[Unit])
  def withToast(text: String, severity: Message.Severity = Message.Severity.Info): F[Unit] =
    f <* ToastCtx[F].showToast(text, severity)

// TODO Move these to react-datetime
extension (instant: Instant)
  // DatePicker only works in local timezone, so we trick it by adding the timezone offset.
  // See https://github.com/Hacker0x01/react-datepicker/issues/1787
  def toDatePickerJsDate: js.Date =
    new js.Date(instant.toEpochMilli.toDouble + (new js.Date()).getTimezoneOffset() * 60000)

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
