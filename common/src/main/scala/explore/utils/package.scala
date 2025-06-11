// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import cats.Applicative
import cats.Endo
import cats.effect.*
import cats.effect.syntax.all.*
import cats.syntax.all.*
import clue.data.*
import clue.data.syntax.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.BuildInfo
import explore.components.ui.ExploreStyles
import fs2.Pipe
import fs2.Pull
import fs2.Stream
import fs2.concurrent.Channel
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ExecutionEnvironment
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.util.NewBoolean
import lucuma.core.util.TimeSpan
import lucuma.react.common.style.Css
import lucuma.ui.components.TimeSpanView
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.versionDateFormatter
import lucuma.ui.utils.versionDateTimeFormatter
import org.scalajs.dom

import java.time.Instant
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

def forceAssign[T, S](mod: Endo[Input[S]] => Endo[T])(base: S): Endo[S] => Endo[T] =
  modS =>
    mod:
      case Assign(edit) => modS(edit).assign
      case _            => modS(base).assign

// DatePicker only works in local timezone, so we trick it by adding the timezone offset.
// See https://github.com/Hacker0x01/react-datepicker/issues/1787
private def fromDatePickerJsDate(jsDate: js.Date): Instant =
  Instant.ofEpochMilli((jsDate.getTime() - jsDate.getTimezoneOffset() * 60000).toLong)

object IsExpanded extends NewBoolean
type IsExpanded = IsExpanded.Type

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

def timeDisplay(
  name:        String,
  time:        TimeSpan,
  sep:         String = ": ",
  timeClass:   TagMod = TagMod.empty,
  timeTooltip: Option[VdomNode] = None
) =
  <.span(<.span(ExploreStyles.SequenceTileTitleItem)(name, sep),
         TimeSpanView(time, tooltip = timeTooltip).withMods(timeClass)
  )

def deriveOptionalEnumerated[A](
  emptyTag: String
)(using e: Enumerated[A]): Enumerated[Option[A]] =
  new Enumerated {
    val all                        = None :: e.all.map(Some(_))
    def tag(oa: Option[A]): String = oa match
      case None    => emptyTag
      case Some(a) => e.tag(a)
  }

def deriveOptionalDisplay[A](
  emptyDisplay: String
)(using d: Display[A]): Display[Option[A]] =
  new Display {
    def shortName(oa: Option[A]): String         = oa match
      case None    => emptyDisplay
      case Some(a) => d.shortName(a)
    override def longName(oa: Option[A]): String = oa match
      case None    => emptyDisplay
      case Some(a) => d.longName(a)
  }
