// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq

/**
 * Holds a set of candidate guide stars
 */
final case class CatalogResults(candidates: List[GuideStarCandidate])

object CatalogResults {
  def empty = CatalogResults(Nil)

  implicit val eqCatalogResults: Eq[CatalogResults] =
    Eq.by(_.candidates)

}
