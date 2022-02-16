// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.implicits._
import io.circe.Decoder
import io.circe.Decoder._
import lucuma.core.model.Observation
import monocle.Focus
import monocle.Lens

import scala.collection.immutable.SortedSet

final case class TargetGroup(obsIds: SortedSet[Observation.Id], targetWithId: TargetWithId) {
  def addObsIds(ids: ObsIdSet): TargetGroup    = TargetGroup.obsIds.modify(_ ++ ids.toSortedSet)(this)
  def removeObsIds(ids: ObsIdSet): TargetGroup =
    TargetGroup.obsIds.modify(_ -- ids.toSortedSet)(this)
}

object TargetGroup {
  implicit val eqTargetGroup: Eq[TargetGroup] = Eq.by(x => (x.obsIds, x.targetWithId))

  val obsIds: Lens[TargetGroup, SortedSet[Observation.Id]] = Focus[TargetGroup](_.obsIds)
  val targetWithId: Lens[TargetGroup, TargetWithId]        = Focus[TargetGroup](_.targetWithId)

  implicit val targetGroupDecode: Decoder[TargetGroup] = Decoder.instance(c =>
    for {
      ids  <- c.get[List[Observation.Id]]("observationIds")
      twid <- c.get[TargetWithId]("target")
    } yield TargetGroup(SortedSet.from(ids), twid)
  )
}
