// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Eq
import cats.syntax.all.*
import explore.model.Constants
import explore.model.TargetWithOptId
import japgolly.scalajs.react.ReactCats.*
import japgolly.scalajs.react.Reusability
import lucuma.catalog.AngularSize
import lucuma.catalog.CatalogTargetResult
import lucuma.core.model.CatalogInfo
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target

case class TargetSearchResult(
  targetWithOptId: TargetWithOptId,
  angularSize:     Option[AngularSize]
) {
  lazy val optId: Option[Target.Id] = targetWithOptId.optId
  lazy val target: Target           = targetWithOptId.target
}

object TargetSearchResult {
  def fromCatalogTargetResult(r: CatalogTargetResult): TargetSearchResult =
    TargetSearchResult(
      TargetWithOptId(
        none,
        Target.Sidereal.sourceProfile
          .andThen(SourceProfile.unnormalizedSED)
          .modify(_.orElse(Constants.DefaultSED.some))(r.target) // Ensure SED is always present.
      ),
      r.angularSize
    )

  given Eq[TargetSearchResult] =
    Eq.by(t =>
      Target.catalogInfo
        .getOption(t.target)
        .flatten
        .map(CatalogInfo.id.get)
        .map(_.value)
        .orElse(Target.ephemerisKey.getOption(t.target).map(_.toString))
        .getOrElse(Target.name.get(t.target).value)
    )

  given Reusability[TargetSearchResult] =
    Reusability.byEq
}
