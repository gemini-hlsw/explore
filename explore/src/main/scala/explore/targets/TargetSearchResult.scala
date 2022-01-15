package explore.targets

import cats.Eq
import cats.syntax.all._
import explore.model.TargetWithOptId
import japgolly.scalajs.react.Reusability
import lucuma.catalog.AngularSize
import lucuma.catalog.CatalogTargetResult
import lucuma.core.model.Target

final case class TargetSearchResult(
  targetWithOptId: TargetWithOptId,
  angularSize:     Option[AngularSize]
) {
  lazy val optId: Option[Target.Id] = targetWithOptId.optId
  lazy val target: Target           = targetWithOptId.target
}

object TargetSearchResult {
  def fromCatalogTargetResult(r: CatalogTargetResult): TargetSearchResult =
    TargetSearchResult(TargetWithOptId(none, r.target), r.angularSize)

  implicit val EqTargetSearchResult: Eq[TargetSearchResult] = Eq.fromUniversalEquals

  implicit val reuseTargetSearchResult: Reusability[TargetSearchResult] =
    Reusability.byEq
}
