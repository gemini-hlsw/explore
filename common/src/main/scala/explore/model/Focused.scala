// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import japgolly.scalajs.react.ReactCats.*
import japgolly.scalajs.react.Reusability
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Target
import monocle.Focus
import monocle.Lens

import scala.collection.immutable.SortedSet

case class Focused(
  obsSet: Option[ObsIdSet] = none,
  target: Option[Target.Id] = none,
  group:  Option[Group.Id] = none
) derives Eq {
  def withObsSetOpt(obsSet: Option[ObsIdSet]): Focused = Focused.obsSet.replace(obsSet)(this)

  def withObsSet(obsSet: ObsIdSet): Focused = withObsSetOpt(obsSet.some)

  def withSingleObs(obsId: Observation.Id): Focused = withObsSet(ObsIdSet.one(obsId))

  def withoutObsSet: Focused = withObsSetOpt(none)

  def withTargetOpt(target: Option[Target.Id]): Focused = Focused.target.replace(target)(this)

  def withTarget(target: Target.Id): Focused = withTargetOpt(target.some)

  def withoutTarget: Focused = withTargetOpt(none)

  // If the current target exists and is in targetIds, keep it. Otherwise set to targetIds.headOption.
  // This keeps the currently focused target selected if possible.
  def validateOrSetTarget(targetIds: SortedSet[Target.Id]): Focused =
    if (target.exists(targetIds.contains)) this else withTargetOpt(targetIds.headOption)

  def withGroupOpt(group: Option[Group.Id]): Focused = Focused.group.replace(group)(this)

  def withGroup(group: Group.Id): Focused = withGroupOpt(group.some)

  def withoutGroup: Focused = withGroupOpt(none)

  def isEmpty: Boolean = obsSet.isEmpty && target.isEmpty
}

object Focused {
  val None: Focused = Focused()

  def obsSet(obsSet: ObsIdSet): Focused = Focused(obsSet.some, none, none)

  def target(target: Target.Id): Focused = Focused(none, target.some, none)

  def singleObs(obsId: Observation.Id, targetId: Option[Target.Id] = none): Focused =
    Focused(ObsIdSet.one(obsId).some, targetId, none)

  def group(group: Group.Id): Focused = Focused(none, none, group.some)

  given Reusability[Focused] = Reusability.byEq

  val obsSet: Lens[Focused, Option[ObsIdSet]] = Focus[Focused](_.obsSet)

  val target: Lens[Focused, Option[Target.Id]] = Focus[Focused](_.target)

  val group: Lens[Focused, Option[Group.Id]] = Focus[Focused](_.group)
}
