// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import lucuma.core.model.Target
import monocle.Focus
import monocle.Lens

import scala.collection.immutable.SortedSet

case class AsterismGroup(
  obsIds:   ObsIdSet,
  asterism: SortedSet[Target.Id]
) {
  // Note: This should only be used while waiting for the server roundtrip
  def addObsIds(newIds: ObsIdSet): AsterismGroup =
    AsterismGroup.obsIds.modify(_ ++ newIds)(this)

  def removeObsIds(toExclude: ObsIdSet): Option[AsterismGroup] =
    this.obsIds.remove(toExclude).map(newIds => this.copy(obsIds = newIds))

  def removeObsIdsUnsafe(toExclude: ObsIdSet): AsterismGroup =
    this.copy(obsIds = this.obsIds.removeUnsafe(toExclude))

  def asObsKeyValue: (ObsIdSet, AsterismGroup) = (this.obsIds, this)
}

object AsterismGroup {
  implicit val eqAsterismGroup: Eq[AsterismGroup] = Eq.by(x => (x.obsIds, x.asterism))

  val obsIds: Lens[AsterismGroup, ObsIdSet]               = Focus[AsterismGroup](_.obsIds)
  val asterism: Lens[AsterismGroup, SortedSet[Target.Id]] = Focus[AsterismGroup](_.asterism)
}
