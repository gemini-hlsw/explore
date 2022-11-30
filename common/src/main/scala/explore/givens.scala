// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.*
import cats.data.NonEmptyList
import cats.effect.Temporal
import cats.effect.kernel.Resource
import cats.syntax.all.*
import clue.*
import crystal.Pot
import crystal.implicits.*
import crystal.react.ReuseViewF
import crystal.react.ReuseViewOptF
import crystal.react.reuse.*
import explore.events.*
import explore.model.AppContext
import explore.optics.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.*
import lucuma.schemas.*
import monocle.function.At
import org.scalajs.dom
import org.typelevel.log4cats.Logger
import queries.schemas.*
import workers.WorkerClient

import scala.annotation.targetName
import scala.annotation.unused
import scala.collection.immutable.SortedMap
import scala.concurrent.duration.*

extension [F[_], A](view: ReuseViewF[F, A])
  def zoomGetAdjust[B](getAdjust: GetAdjust[A, B])(implicit F: Monad[F]): ReuseViewF[F, B] =
    view.zoom(getAdjust.get)(getAdjust.mod)

  // Helps type inference by sidestepping overloaded "zoom".
  def zoomPrism[B](prism: monocle.Prism[A, B])(implicit F: Monad[F]): ReuseViewOptF[F, B] =
    view.zoom(prism)

  @targetName("ReuseView_zoomLens")
  // Helps type inference by sidestepping overloaded "zoom".
  def zoomLens[B](lens: monocle.Lens[A, B])(implicit F: Monad[F]): ReuseViewF[F, B] =
    view.zoom(lens)

extension [F[_], A](viewOpt: ReuseViewOptF[F, A])
  @targetName("ReuseViewOpt_zoomLens")
  // Helps type inference by sidestepping overloaded "zoom".
  def zoomLens[B](lens: monocle.Lens[A, B])(implicit F: Monad[F]): ReuseViewOptF[F, B] =
    viewOpt.zoom(lens)

// React implicits
extension (a: HtmlAttrs)
  // Generalize https://gist.github.com/pstoica/4323d3e6e37e8a23dd59
  def onComponentBlur(handler: Callback): TagMod =
    a.onBlur ==> { (e: ReactFocusEvent) =>
      val currentTarget = e.currentTarget

      handler
        .when_(!currentTarget.contains(dom.document.activeElement))
        .setTimeout(Duration.Zero)
        .void
    }

extension [F[_], A](f: F[A])
  def attemptPot(implicit F: MonadThrow[F]): F[Pot[A]] = f.attempt.map(_.toTry.some.toPot)

  /**
   * Given an effect producing an A and a signal stream, runs the effect and then re-runs it whenver
   * a signal is received, producing a Stream[A].
   */
  def reRunOnSignal(
    signal:   fs2.Stream[F, Unit],
    debounce: Option[FiniteDuration] = 2.seconds.some
  )(using Temporal[F]): fs2.Stream[F, A] = {
    val debouncedSignal = debounce.fold(signal)(signal.debounce)
    fs2.Stream.eval(f) ++ debouncedSignal.evalMap(_ => f)
  }

  /**
   * Given an effect producing an A and a bunch of signal streams, runs the effect and then re-runs
   * it whenver a signal is received, producing a Stream[A].
   */
  def reRunOnSignals(
    signals:  NonEmptyList[fs2.Stream[F, Unit]],
    debounce: Option[FiniteDuration] = 2.seconds.some
  )(using Temporal[F]): fs2.Stream[F, A] =
    reRunOnSignal(signals.reduceLeft(_ merge _), debounce)

  def reRunOnResourceSignals(
    subscriptions: NonEmptyList[Resource[F, fs2.Stream[F, ?]]],
    debounce:      Option[FiniteDuration] = 2.seconds.some
  )(using Temporal[F]): Resource[F, fs2.Stream[F, A]] =
    subscriptions.sequence
      .map(ss => reRunOnSignals(ss.map(_.void), debounce))

  def reRunOnResourceSignals(
    head: Resource[F, fs2.Stream[F, ?]],
    tail: Resource[F, fs2.Stream[F, ?]]*
  )(using Temporal[F]): Resource[F, fs2.Stream[F, A]] =
    reRunOnResourceSignals(
      NonEmptyList.of(head, tail: _*),
      2.seconds.some // For some reason, compiler can't resolve default parameter value here
    )

  def reRunOnResourceSignals(
    debounce: FiniteDuration,
    head:     Resource[F, fs2.Stream[F, ?]],
    tail:     Resource[F, fs2.Stream[F, ?]]*
  )(using Temporal[F]): Resource[F, fs2.Stream[F, A]] =
    reRunOnResourceSignals(NonEmptyList.of(head, tail: _*), debounce.some)

extension [F[_], A](f: F[Pot[A]])
  def resetOnSignal(
    signal:   fs2.Stream[F, Unit],
    debounce: Option[FiniteDuration] = 2.seconds.some
  )(using Temporal[F]): fs2.Stream[F, Pot[A]] = {
    val debouncedSignal = debounce.fold(signal)(signal.debounce)
    fs2.Stream.eval(f) ++ debouncedSignal.flatMap(_ =>
      fs2.Stream(Pot.pending) ++ fs2.Stream.eval(f)
    )
  }

  def resetOnSignals(
    signals:  NonEmptyList[fs2.Stream[F, Unit]],
    debounce: Option[FiniteDuration] = 2.seconds.some
  )(using Temporal[F]): fs2.Stream[F, Pot[A]] =
    resetOnSignal(signals.reduceLeft(_ merge _), debounce)

  private def resetOnResourceSignalsB(
    subscriptions: NonEmptyList[Resource[F, fs2.Stream[F, ?]]],
    debounce:      Option[FiniteDuration] = 2.seconds.some
  )(using Temporal[F]): Resource[F, fs2.Stream[F, Pot[A]]] =
    subscriptions.sequence
      .map(ss => resetOnSignals(ss.map(_.void), debounce))

  def resetOnResourceSignals(
    head: Resource[F, fs2.Stream[F, ?]],
    tail: Resource[F, fs2.Stream[F, ?]]*
  )(using Temporal[F]): Resource[F, fs2.Stream[F, Pot[A]]] =
    resetOnResourceSignalsB(NonEmptyList.of(head, tail: _*))

  def resetOnResourceSignals(
    debounce: FiniteDuration,
    head:     Resource[F, fs2.Stream[F, ?]],
    tail:     Resource[F, fs2.Stream[F, ?]]*
  )(using Temporal[F]): Resource[F, fs2.Stream[F, Pot[A]]] =
    resetOnResourceSignalsB(NonEmptyList.of(head, tail: _*), debounce.some)
