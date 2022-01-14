// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import explore.model.ObsSummary
import monocle.Lens
import monocle.macros.GenLens

import java.util.UUID

sealed trait ObsNode {
  val id: UUID
}
object ObsNode       {
  final case class Obs(id: UUID, value: ObsSummary) extends ObsNode

  object Obs {
    val id: Lens[Obs, UUID] = GenLens[Obs](_.id)
  }

  final case class And(id: UUID, andParams: String) extends ObsNode
  object And {
    val id: Lens[And, UUID] = GenLens[And](_.id)
  }

  final case class Or(id: UUID, orParams: String) extends ObsNode
  object Or {
    val id: Lens[Or, UUID] = GenLens[Or](_.id)
  }

  val id: Lens[ObsNode, UUID] = Lens[ObsNode, UUID](_.id)(id =>
    _ match {
      case obs @ Obs(_, _) => Obs.id.replace(id)(obs)
      case and @ And(_, _) => And.id.replace(id)(and)
      case or @ Or(_, _)   => Or.id.replace(id)(or)
    }
  )
}
