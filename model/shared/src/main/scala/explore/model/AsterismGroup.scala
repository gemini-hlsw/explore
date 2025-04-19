// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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

case class AsterismGroup(obsIds: ObsIdSet, targetIds: SortedSet[Target.Id]) derives Eq

object AsterismGroup:
  def fromTuple(obsIds: ObsIdSet, targetIds: SortedSet[Target.Id]): AsterismGroup =
    AsterismGroup(obsIds, targetIds)

  given Semigroup[AsterismGroup] =
    Semigroup.instance((a, b) => AsterismGroup(a.obsIds |+| b.obsIds, a.targetIds |+| b.targetIds))

  val obsIds: Lens[AsterismGroup, ObsIdSet] = Focus[AsterismGroup](_.obsIds)

  val targetIds: Lens[AsterismGroup, SortedSet[Target.Id]] = Focus[AsterismGroup](_.targetIds)
