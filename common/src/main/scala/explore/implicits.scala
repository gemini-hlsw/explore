// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats._
import cats.data.NonEmptyList
import cats.effect.Temporal
import cats.effect.kernel.Resource
import cats.syntax.all._
import clue._
import coulomb.Quantity
import crystal.Pot
import crystal.ViewF
import crystal.ViewOptF
import crystal.implicits._
import crystal.react.ReuseViewF
import crystal.react.ReuseViewOptF
import crystal.react.reuse._
import eu.timepit.refined.api.Refined
import explore.model.AppContext
import explore.optics._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom._
import lucuma.core.optics._
import lucuma.core.util.Enumerated
import lucuma.schemas._
import monocle.function.At.at
import monocle.function.Index.index
import org.scalajs.dom
import org.typelevel.log4cats.Logger
import queries.schemas._
import shapeless._

import scala.annotation.unused
import scala.collection.immutable.SortedMap
import scala.concurrent.duration._

trait ListImplicits {

  // Adapted from https://stackoverflow.com/a/21444327/5808801
  implicit object hnilMonoid extends Monoid[HNil] {
    val empty                                       = HNil
    def combine(@unused f1: HNil, @unused f2: HNil) = HNil
  }

  implicit def hconsMonoid[H: Monoid, T <: HList: Monoid] =
    new Monoid[H :: T] {
      val empty                           = Monoid[H].empty :: Monoid[T].empty
      def combine(f1: H :: T, f2: H :: T) =
        (f1.head |+| f2.head) :: (f1.tail |+| f2.tail)
    }

  private object singleton extends Poly1 { implicit def anything[A] = at[A](List(_)) }

  implicit class UnzipListOpts[L <: HList](hlists: List[L]) {
    def unzipN[Out <: HList](implicit
      mapper: ops.hlist.Mapper.Aux[singleton.type, L, Out],
      monoid: Monoid[Out]
    ): Out = hlists.map(_.map(singleton)).combineAll
  }

  implicit class ViewListOps[F[_]: Monad, A](val viewList: ViewF[F, List[A]]) {
    def toListOfViews: List[ViewF[F, A]] =
      // It's safe to "get" since we are only invoking for existing indices.
      viewList.get.indices.toList.map { i =>
        val atIndex = index[List[A], Int, A](i)
        viewList
          .zoom((atIndex.getOption _).andThen(_.get))(atIndex.modify)
      }
  }

  implicit class ViewMapOps[F[_]: Monad, K, V](val viewMap: ViewF[F, Map[K, V]]) {
    def toListOfViews: List[(K, ViewF[F, V])] =
      // It's safe to "get" since we are only invoking for existing keys.
      viewMap.get.keys.toList.map(k =>
        k -> viewMap.zoom(at[Map[K, V], K, Option[V]](k)).zoom(_.get)(f => _.map(f))
      )
  }

  implicit class ListOps[A](val list: List[A]) {
    def toSortedMap[K: Ordering, V](getKey: A => K, getValue: A => V = identity[A](_)) =
      SortedMap.from(list.map(a => (getKey(a), getValue(a))))
  }
}

trait ContextImplicits {
  implicit def appContext2Parallel[F[_]](implicit ctx: AppContext[F]): Parallel[F] =
    ctx.P
  implicit def appContext2Logger[F[_]](implicit ctx: AppContext[F]): Logger[F]     =
    ctx.logger
  implicit def appContext2UserPreferencesDBClient[F[_]](implicit
    ctx: AppContext[F]
  ): WebSocketClient[F, UserPreferencesDB] =
    ctx.clients.preferencesDB
  implicit def appContext2ODBClient[F[_]](implicit
    ctx: AppContext[F]
  ): WebSocketClient[F, ObservationDB] =
    ctx.clients.odb
  implicit def appContext2ITC[F[_]](implicit
    ctx: AppContext[F]
  ): TransactionalClient[F, ITC] =
    ctx.clients.itc
}

trait RefinedImplicits {
  @inline
  implicit def vdomNodeFromRefined[T, P](v: T Refined P)(implicit
    f:                                      T => VdomNode
  ): VdomNode =
    f(v.value)
}

trait EnumeratedImplicits {
  implicit def zipEnums[A: Enumerated, B: Enumerated]: Enumerated[(A, B)] =
    Enumerated
      .fromNEL(NonEmptyList.fromListUnsafe((Enumerated[A].all, Enumerated[B].all).tupled))
      .withTag { case (a, b) => s"${Enumerated[A].tag(a)}, ${Enumerated[B].tag(b)} " }
}

object implicits
    extends ShorthandTypes
    with ListImplicits
    with ContextImplicits
    with RefinedImplicits
    with EnumeratedImplicits {
  // View Optics implicits
  implicit class ViewOpticsOps[F[_], A](val view: ReuseViewF[F, A]) extends AnyVal {
    def zoomGetAdjust[B](getAdjust: GetAdjust[A, B])(implicit F: Monad[F]): ReuseViewF[F, B] =
      view.zoom(getAdjust.get)(getAdjust.mod)

    // Helps type inference by sidestepping overloaded "zoom".
    def zoomPrism[B](prism: monocle.Prism[A, B])(implicit F: Monad[F]): ReuseViewOptF[F, B] =
      view.zoom(prism)

    // Helps type inference by sidestepping overloaded "zoom".
    def zoomLens[B](lens: monocle.Lens[A, B])(implicit F: Monad[F]): ReuseViewF[F, B] =
      view.zoom(lens)
  }

  implicit class ViewOptOpticsOps[F[_], A](val viewOpt: ReuseViewOptF[F, A]) extends AnyVal {
    // Helps type inference by sidestepping overloaded "zoom".
    def zoomLens[B](lens: monocle.Lens[A, B])(implicit F: Monad[F]): ReuseViewOptF[F, B] =
      viewOpt.zoom(lens)
  }

  // Coulomb implicits
  implicit class CoulombViewOps[F[_], N, U](val self: ViewF[F, Quantity[N, U]]) extends AnyVal {
    def stripQuantity: ViewF[F, N] = self.as(quantityIso[N, U])
  }

  implicit class CoulombViewOptOps[F[_], N, U](val self: ViewOptF[F, Quantity[N, U]])
      extends AnyVal {
    def stripQuantity: ViewOptF[F, N] = self.as(quantityIso[N, U])
  }

  implicit class CoulombReuseViewOptOps[F[_], N, U](val self: ReuseViewOptF[F, Quantity[N, U]])
      extends AnyVal {
    def stripQuantity(implicit F: Monad[F]): ReuseViewOptF[F, N] = self.as(quantityIso[N, U])
  }

  // React implicits
  implicit class HtmlAttrsOps(val a: HtmlAttrs) extends AnyVal {
    // Generalize https://gist.github.com/pstoica/4323d3e6e37e8a23dd59
    def onComponentBlur(handler: Callback): TagMod =
      a.onBlur ==> { (e: ReactFocusEvent) =>
        val currentTarget = e.currentTarget

        handler
          .when_(!currentTarget.contains(dom.document.activeElement))
          .setTimeout(Duration.Zero)
          .void
      }
  }

  implicit class EffectOps[F[_], A](val f: F[A]) extends AnyVal {

    def attemptPot(implicit F: MonadThrow[F]): F[Pot[A]] = f.attempt.map(_.toTry.some.toPot)

    /**
     * Given an effect producing an A and a signal stream, runs the effect and then re-runs it
     * whenver a signal is received, producing a Stream[A].
     */
    def reRunOnSignal(
      signal:     fs2.Stream[F, Unit],
      debounce:   Option[FiniteDuration] = 2.seconds.some
    )(implicit F: Temporal[F]): fs2.Stream[F, A] = {
      val debouncedSignal = debounce.fold(signal)(signal.debounce)
      fs2.Stream.eval(f) ++ debouncedSignal.evalMap(_ => f)
    }

    /**
     * Given an effect producing an A and a bunch of signal streams, runs the effect and then
     * re-runs it whenver a signal is received, producing a Stream[A].
     */
    def reRunOnSignals(
      signals:    NonEmptyList[fs2.Stream[F, Unit]],
      debounce:   Option[FiniteDuration] = 2.seconds.some
    )(implicit F: Temporal[F]): fs2.Stream[F, A] =
      reRunOnSignal(signals.reduceLeft(_ merge _), debounce)

    def reRunOnResourceSignals(
      subscriptions: NonEmptyList[Resource[F, fs2.Stream[F, _]]],
      debounce:      Option[FiniteDuration] = 2.seconds.some
    )(implicit F:    Temporal[F]): Resource[F, fs2.Stream[F, A]] =
      subscriptions.sequence
        .map(ss => reRunOnSignals(ss.map(_.void), debounce))

    def reRunOnResourceSignals(
      head:       Resource[F, fs2.Stream[F, _]],
      tail:       Resource[F, fs2.Stream[F, _]]*
    )(implicit F: Temporal[F]): Resource[F, fs2.Stream[F, A]] =
      reRunOnResourceSignals(NonEmptyList.of(head, tail: _*))

    def reRunOnResourceSignals(
      debounce:   FiniteDuration,
      head:       Resource[F, fs2.Stream[F, _]],
      tail:       Resource[F, fs2.Stream[F, _]]*
    )(implicit F: Temporal[F]): Resource[F, fs2.Stream[F, A]] =
      reRunOnResourceSignals(NonEmptyList.of(head, tail: _*), debounce.some)
  }

  implicit class PotEffectOps[F[_], A](val f: F[Pot[A]]) extends AnyVal {
    def resetOnSignal(
      signal:     fs2.Stream[F, Unit],
      debounce:   Option[FiniteDuration] = 2.seconds.some
    )(implicit F: Temporal[F]): fs2.Stream[F, Pot[A]] = {
      val debouncedSignal = debounce.fold(signal)(signal.debounce)
      fs2.Stream.eval(f) ++ debouncedSignal.flatMap(_ =>
        fs2.Stream(Pot.pending) ++ fs2.Stream.eval(f)
      )
    }

    def resetOnSignals(
      signals:    NonEmptyList[fs2.Stream[F, Unit]],
      debounce:   Option[FiniteDuration] = 2.seconds.some
    )(implicit F: Temporal[F]): fs2.Stream[F, Pot[A]] =
      resetOnSignal(signals.reduceLeft(_ merge _), debounce)

    def resetOnResourceSignals(
      subscriptions: NonEmptyList[Resource[F, fs2.Stream[F, _]]],
      debounce:      Option[FiniteDuration] = 2.seconds.some
    )(implicit F:    Temporal[F]): Resource[F, fs2.Stream[F, Pot[A]]] =
      subscriptions.sequence
        .map(ss => resetOnSignals(ss.map(_.void), debounce))

    def resetOnResourceSignals(
      head:       Resource[F, fs2.Stream[F, _]],
      tail:       Resource[F, fs2.Stream[F, _]]*
    )(implicit F: Temporal[F]): Resource[F, fs2.Stream[F, Pot[A]]] =
      resetOnResourceSignals(NonEmptyList.of(head, tail: _*))

    def resetOnResourceSignals(
      debounce:   FiniteDuration,
      head:       Resource[F, fs2.Stream[F, _]],
      tail:       Resource[F, fs2.Stream[F, _]]*
    )(implicit F: Temporal[F]): Resource[F, fs2.Stream[F, Pot[A]]] =
      resetOnResourceSignals(NonEmptyList.of(head, tail: _*), debounce.some)
  }
}
