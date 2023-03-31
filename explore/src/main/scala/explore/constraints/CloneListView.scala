// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.constraints

import japgolly.scalajs.react.util.DefaultEffects.{Sync => DefaultS}
import crystal.ViewListF
import crystal.ViewF
import cats.Monad
import cats.data.NonEmptyList

// Move to crystal
type ViewList[A] = ViewListF[DefaultS, A]

// final class ViewF[F[_]: Monad, A](val get: A, val modCB: (A => A, A => F[Unit]) => F[Unit])
//     extends ViewOps[F, Id, A] { self =>

// View into a non-empty list of cloned objects.
// Allows treating them as a single object.
// final class CloneListView[F[_]: Monad, A](val underlying: ViewF[F, NonEmptyList[A]])
//     extends ViewF[F, A](
//       get = underlying.get.head,
//       modCB = (mod, cb) => underlying.modCB(_.map(mod), l => cb(l.head))
//     )

// Turn a ViewList into a View, assuming all returned objects are clones,
// and therefore just returning the head upon get.
final class CloneListView[F[_]: Monad, A](val underlying: ViewListF[F, A])
    extends ViewF[F, A](
      get = underlying.get.head,
      modCB = (mod, cb) => underlying.modCB(mod, l => cb(l.head))
    )
