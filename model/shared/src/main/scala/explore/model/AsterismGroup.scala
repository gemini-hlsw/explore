// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.Semigroup
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.model.Target
import monocle.Focus
import monocle.Lens

import scala.collection.immutable.SortedSet

case class AsterismGroup(obsIds: ObsIdSet, targetIds: SortedSet[Target.Id]) derives Eq:
  def addTargetId(targetId: Target.Id): AsterismGroup =
    AsterismGroup.targetIds.modify(_ + targetId)(this)

  def addTargetIds(targetIds: Set[Target.Id]): AsterismGroup =
    AsterismGroup.targetIds.modify(_ ++ targetIds)(this)

  def addObsIds(newIds: ObsIdSet): AsterismGroup =
    AsterismGroup.obsIds.modify(_ ++ newIds)(this)

  def removeObsIds(toExclude: ObsIdSet): Option[AsterismGroup] =
    this.obsIds.remove(toExclude).map(newIds => this.copy(obsIds = newIds))

  def removeObsIdsUnsafe(toExclude: ObsIdSet): AsterismGroup =
    this.copy(obsIds = this.obsIds.removeUnsafe(toExclude))

  def asObsKeyValue: (ObsIdSet, AsterismGroup) = (this.obsIds, this)

object AsterismGroup:
  given Semigroup[AsterismGroup] =
    Semigroup.instance((a, b) => AsterismGroup(a.obsIds |+| b.obsIds, a.targetIds |+| b.targetIds))

  val obsIds: Lens[AsterismGroup, ObsIdSet] = Focus[AsterismGroup](_.obsIds)

  val targetIds: Lens[AsterismGroup, SortedSet[Target.Id]] = Focus[AsterismGroup](_.targetIds)
