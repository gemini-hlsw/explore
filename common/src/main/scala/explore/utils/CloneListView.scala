// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import cats.Monad
import crystal.ViewF
import crystal.ViewListF

// Turn a ViewList into a View, assuming all returned objects are clones.
// and therefore just returning the head upon get.
final class CloneListView[F[_]: Monad, A](val underlying: ViewListF[F, A])
    extends ViewF[F, A](
      get = underlying.get.head,
      modCB = (mod, cb) =>
        val previous = underlying.get.head
        underlying.modCB(mod, l => cb(previous, l.head))
    )
