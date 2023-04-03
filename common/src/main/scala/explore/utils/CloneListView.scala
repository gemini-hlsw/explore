// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import japgolly.scalajs.react.util.DefaultEffects.{Sync => DefaultS}
import crystal.ViewListF
import crystal.ViewF
import cats.Monad
// import cats.data.NonEmptyList

// TODO Move to crystal
// type ViewList[A] = ViewListF[DefaultS, A]

// View into a non-empty list of cloned objects.
// Allows treating them as a single object.
// final class CloneListView[F[_]: Monad, A](val underlying: ViewF[F, Iterable[A]])
//     extends ViewF[F, A](
//       get = underlying.get.head,
//       modCB = (mod, cb) => underlying.modCB(_.map(mod), l => cb(l.head))
//     )

// Turn a ViewList into a View, assuming all returned objects are clones.
// and therefore just returning the head upon get.
final class CloneListView[F[_]: Monad, A](val underlying: ViewListF[F, A])
    extends ViewF[F, A](
      get = underlying.get.head,
      modCB = (mod, cb) => underlying.modCB(mod, l => cb(l.head))
    )
