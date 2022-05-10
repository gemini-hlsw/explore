// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import java.time.Instant

final case class CatalogResults(candidates: List[GuideStarCandidate], date: Instant)
object CatalogResults {
  def empty = CatalogResults(Nil, Instant.now)

}
