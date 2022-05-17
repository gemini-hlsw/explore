// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import org.typelevel.cats.time._

import java.time.Instant

/**
 * Holds a set of candidate guide stars and a reference date for expiration
 */
final case class CatalogResults(candidates: List[GuideStarCandidate], referenceDate: Instant)

object CatalogResults {
  def empty = CatalogResults(Nil, Instant.now)

  implicit val eqCatalogResults: Eq[CatalogResults] =
    Eq.by(x => (x.candidates, x.referenceDate))

}
