// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.util

import cats.syntax.all.given
import cats.effect.Concurrent
import cats.effect.Ref
import cats.effect.Resource
import scala.collection.immutable.SortedMap
import fs2.concurrent.SignallingRef
import cats.effect.kernel.Deferred

class Cache[F[_], K, V](map: SignallingRef[F, SortedMap[K, V]]): // , fetch: K => F[Option[V]]):
  def stream: fs2.Stream[F, SortedMap[K, V]] = map.discrete

  def update(delta: SortedMap[K, Option[V]]): F[Unit] =
    map.update(m =>
      delta.foldLeft(m) { case (m, (k, vOpt)) => vOpt.fold(m.removed(k))(m.updated(k, _)) }
    )

  def update(key: K, value: Option[V])(using Ordering[K]): F[Unit] =
    update(SortedMap(key -> value))

object Cache:
  def init[F[_]: Concurrent, K, V](map: SortedMap[K, V]): F[Cache[F, K, V]] =
    SignallingRef[F].of(map).map(new Cache(_))

// Elements can be added and removed. Fetching can result in errors.
// Subscriptions can be missed (in case of reconnection, restart cache).

// class SelfUpdatingCache[F[_], K, V](
//   map:          SignallingRef[F, SortedMap[K, V]],
//   updateStream: fs2.Stream[F, SortedMap[K, Option[V]]]
// ) extends Cache(map)

object SelfUpdatingCache:
  // By using effectful init and resourceful stream, we can ensure that the subscription starts
  // before the initial query, thus not missing updates that can happen immediately after the initial query.
  def init[F[_]: Concurrent, K, V](
    initial:      F[SortedMap[K, V]],
    updateStream: Resource[F, fs2.Stream[F, SortedMap[K, Option[V]]]]
  ): F[Cache[F, K, V]] =
    for
      latch        <- Deferred[F, Cache[F, K, V]]
      _            <- updateStream
                        .evalMap(stream =>
                          latch.get.flatMap(cache =>
                            stream.evalMap(delta => cache.update(delta)).compile.drain
                          )
                        )
                        .useForever
      initialValue <- initial
      cache        <- Cache.init(initialValue)
      _            <- latch.complete(cache) // Allow stream updates to proceed
    yield cache
