// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.implicits._
import gem.Target
import monocle.Lens
import monocle.macros.Lenses

@Lenses
final case class ExploreTarget(searchTerm: String, target: Option[Target])

object ExploreTarget {
  val searchTermL: Lens[Option[ExploreTarget], String] =
    Lens[Option[ExploreTarget], String](_.map(_.searchTerm).orEmpty)(b =>
      s => s.map(_.copy(searchTerm = b))
    )
}
