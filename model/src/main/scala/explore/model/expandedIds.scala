package explore.model

import cats._
import cats.Order._
import scala.collection.immutable.SortedSet
import lucuma.core.model.Target
import lucuma.core.model.Asterism
import monocle.macros.Lenses

@Lenses
case class TargetViewExpandedIds(
  targetIds:   SortedSet[Target.Id] = SortedSet.empty,
  asterismIds: SortedSet[Asterism.Id] = SortedSet.empty
)

object TargetViewExpandedIds {
  implicit val eqTargetViewExpandedIds: Eq[TargetViewExpandedIds] =
    Eq.by(m => (m.targetIds, m.asterismIds))
}
