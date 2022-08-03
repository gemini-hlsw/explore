// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.syntax.all._
import explore.model.ObsIdSet
import japgolly.scalajs.react.ReactCats._
import japgolly.scalajs.react.Reusability
import lucuma.core.model.Observation
import lucuma.core.model.Target
import monocle.Focus
import monocle.Lens

final case class Focused(obsSet: Option[ObsIdSet] = none, target: Option[Target.Id] = none) {
  def withObsSetOpt(obsSet: Option[ObsIdSet]): Focused = Focused.obsSet.replace(obsSet)(this)

  def withObsSet(obsSet: ObsIdSet): Focused = withObsSetOpt(obsSet.some)

  def withSingleObs(obsId: Observation.Id): Focused = withObsSet(ObsIdSet.one(obsId))

  def withoutObsSet: Focused = withObsSetOpt(none)

  def withTargetOpt(target: Option[Target.Id]): Focused = Focused.target.replace(target)(this)

  def withTarget(target: Target.Id): Focused = withTargetOpt(target.some)

  def withoutTarget: Focused = withTargetOpt(none)
}

object Focused {
  val None: Focused = Focused()

  def obsSet(obsSet: ObsIdSet): Focused = Focused(obsSet.some, none)

  def target(target: Target.Id): Focused = Focused(none, target.some)

  def singleObs(obsId: Observation.Id, targetId: Option[Target.Id] = none): Focused =
    Focused(ObsIdSet.one(obsId).some, targetId)

  implicit val eqFocused: Eq[Focused] = Eq.by(x => (x.obsSet, x.target))

  implicit val reuseFocused: Reusability[Focused] = Reusability.byEq

  val obsSet: Lens[Focused, Option[ObsIdSet]] = Focus[Focused](_.obsSet)

  val target: Lens[Focused, Option[Target.Id]] = Focus[Focused](_.target)
}
