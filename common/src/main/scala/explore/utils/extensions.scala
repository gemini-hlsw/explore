// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import cats.FlatMap
import cats.Monoid
import cats.Semigroup
import cats.effect.Clock
import cats.effect.IO
import cats.effect.Sync
import cats.effect.Temporal
import cats.effect.std.UUIDGen
import cats.effect.syntax.all.*
import cats.syntax.all.*
import crystal.Deglitcher
import crystal.react.syntax.effect.*
import eu.timepit.refined.types.numeric.NonNegLong
import eu.timepit.refined.types.numeric.NonNegShort
import explore.components.ui.ExploreStyles
import fs2.Chunk
import fs2.Pull
import fs2.Stream
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.fa.IconSize
import lucuma.react.primereact.Button
import lucuma.react.primereact.Message
import lucuma.react.primereact.MessageItem
import lucuma.react.primereact.ToastRef
import lucuma.ui.LucumaIcons
import lucuma.ui.LucumaStyles
import org.http4s.Uri
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.*

extension (uri: Uri)
  def addPath(p: Uri.Path): Uri =
    uri.withPath(uri.path.concat(p))

extension (f: Callback)
  def showToastCB(text: String, severity: Message.Severity = Message.Severity.Info)(using
    ToastCtx[IO],
    Logger[IO]
  ): Callback =
    f.toAsync.withToast(text, severity).runAsync

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

  def throttle(duration: FiniteDuration)(using Temporal[F]): fs2.Stream[F, O] =
    fs2.Stream
      .eval(Deglitcher[F](duration))
      .flatMap: deglitcher =>
        s.through(deglitcher.debounce).evalTap(_ => deglitcher.throttle)

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

extension [F[_]: Sync: ToastCtx](f: F[Unit])
  def withToast(
    text:     String,
    severity: Message.Severity = Message.Severity.Info,
    sticky:   Boolean = false
  ): F[Unit] =
    f <* ToastCtx[F].showToast(text, severity, sticky)

  def withToastBefore(
    text:     String,
    severity: Message.Severity = Message.Severity.Info,
    sticky:   Boolean = false
  ): F[Unit] =
    ToastCtx[F].showToast(text, severity, sticky) *> f

  def withToastDuring(
    text:         String,
    completeText: Option[String] = none,
    errorText:    Option[String] = none
  )(using UUIDGen[F], Monoid[F[Unit]]): F[Unit] =
    ToastCtx[F].showToastDuring(text, completeText, errorText).use(_ => f)

  def clearToastsAfter: F[Unit] =
    f <* ToastCtx[F].clear()

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

extension (s: NonNegShort)
  def |+|(i: Int): NonNegShort =
    NonNegShort.unsafeFrom:
      val int = s.value + i
      if int > Short.MaxValue then Short.MaxValue
      else if int < 0 then 0
      else int.toShort

  def |-|(i: Int): NonNegShort =
    s |+| -i

extension [F[_], A](effect: F[A])
  def logTime(name: String)(using Clock[F], FlatMap[F], Logger[F]): F[A] =
    effect.timed.flatMap: (d, a) =>
      Logger[F].debug(s"***** $name took ${d.toSeconds}s (${d.toMillis}ms)").as(a)
