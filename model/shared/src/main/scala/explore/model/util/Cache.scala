// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.util

import cats.syntax.all.given
import cats.effect.syntax.all.given
import cats.effect.Concurrent
import cats.effect.Ref
import cats.effect.Resource
import fs2.concurrent.SignallingRef
import cats.effect.kernel.Deferred

// Maybe we don't need this. It's just a wrapper over SignallingRef.
class Cache[F[_], A](ref: SignallingRef[F, A]):
  def stream: fs2.Stream[F, A] = ref.discrete

  def update(mod: A => A): F[Unit] =
    ref.update(mod)

  // def update(delta: SortedMap[K, Option[V]]): F[Unit] =
  //   map.update(m =>
  //     delta.foldLeft(m) { case (m, (k, vOpt)) => vOpt.fold(m.removed(k))(m.updated(k, _)) }
  //   )

  // def update(key: K, value: Option[V])(using Ordering[K]): F[Unit] =
  //   update(SortedMap(key -> value))

object Cache:
  def init[F[_]: Concurrent, A](value: A): F[Cache[F, A]] =
    SignallingRef[F].of(value).map(new Cache(_))

// Elements can be added and removed. Fetching can result in errors.
// Subscriptions can be missed (in case of reconnection, restart cache).

// class SelfUpdatingCache[F[_], K, V](
//   map:          SignallingRef[F, SortedMap[K, V]],
//   updateStream: fs2.Stream[F, SortedMap[K, Option[V]]]
// ) extends Cache(map)

object SelfUpdatingCache:
  // By using effectful init and resourceful stream, we can ensure that the subscription starts
  // before the initial query, thus not missing updates that can happen immediately after the initial query.
  def init[F[_]: Concurrent, A](
    initial:      F[A],
    updateStream: Resource[F, fs2.Stream[F, A => A]]
  ): F[Cache[F, A]] =
    for
      latch        <- Deferred[F, Cache[F, A]]
      _            <-                       // Start the update fiber. Will only update once cache is initialized.
        updateStream
          .evalTap(
            _.evalTap(mod => latch.get.flatMap(_.update(mod))).compile.drain
          )
          .useForever
          .start
      initialValue <- initial
      cache        <- Cache.init(initialValue)
      _            <- latch.complete(cache) // Allow stream updates to proceed.
    yield cache
