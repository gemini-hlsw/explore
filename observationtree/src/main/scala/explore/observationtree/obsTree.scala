// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import java.util.UUID

import explore.model.ExploreObservation
import gem.Observation
import gem.Target
import monocle.Lens
import monocle.macros.Lenses

sealed trait ObsNode {
  val id: UUID
}
object ObsNode       {
  @Lenses
  final case class Obs(id: UUID, value: ExploreObservation) extends ObsNode
  @Lenses
  final case class And(id: UUID, andParams: String) extends ObsNode
  @Lenses
  final case class Or(id: UUID, orParams: String) extends ObsNode

  val id: Lens[ObsNode, UUID] = Lens[ObsNode, UUID](_.id)(id =>
    _ match {
      case obs @ Obs(_, _) => Obs.id.set(id)(obs)
      case and @ And(_, _) => And.id.set(id)(and)
      case or @ Or(_, _)   => Or.id.set(id)(or)
    }
  )
}
