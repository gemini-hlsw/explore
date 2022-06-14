// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import explore.common.AsterismQueries.AsterismGroupsWithObs
import explore.common.ConstraintGroupQueries.ConstraintGroupList
import explore.common.ObsQueries.ObservationData
import explore.common.ObsQueries.ObservationList
import explore.model.ObsIdSet
import explore.undo.UndoStacks
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.core.model.Proposal
import lucuma.core.model.Target
import monocle.Focus

case class ModelUndoStacks[F[_]](
  forObsList:           UndoStacks[F, ObservationList] = UndoStacks.empty[F, ObservationList],
  forAsterismGroupList: UndoStacks[F, AsterismGroupsWithObs] =
    UndoStacks.empty[F, AsterismGroupsWithObs],
  forSiderealTarget:    Map[Target.Id, UndoStacks[F, Target.Sidereal]] =
    Map.empty[Target.Id, UndoStacks[F, Target.Sidereal]],
  forConstraintList:    UndoStacks[F, ConstraintGroupList] = UndoStacks.empty[F, ConstraintGroupList],
  forConstraintGroup:   Map[ObsIdSet, UndoStacks[F, ConstraintSet]] =
    Map.empty[ObsIdSet, UndoStacks[F, ConstraintSet]],
  forObservationData:   Map[Observation.Id, UndoStacks[F, ObservationData]] =
    Map.empty[Observation.Id, UndoStacks[F, ObservationData]],
  forProposal:          UndoStacks[F, Proposal] = UndoStacks.empty[F, Proposal]
)

object ModelUndoStacks {
  def forObsList[F[_]]           = Focus[ModelUndoStacks[F]](_.forObsList)
  def forAsterismGroupList[F[_]] = Focus[ModelUndoStacks[F]](_.forAsterismGroupList)
  def forSiderealTarget[F[_]]    = Focus[ModelUndoStacks[F]](_.forSiderealTarget)
  def forConstraintList[F[_]]    = Focus[ModelUndoStacks[F]](_.forConstraintList)
  def forConstraintGroup[F[_]]   = Focus[ModelUndoStacks[F]](_.forConstraintGroup)
  def forObservationData[F[_]]   = Focus[ModelUndoStacks[F]](_.forObservationData)
  def forProposal[F[_]]          = Focus[ModelUndoStacks[F]](_.forProposal)

  implicit def eqModelUndoStacks[F[_]]: Eq[ModelUndoStacks[F]] =
    Eq.by(u =>
      (u.forObsList,
       u.forAsterismGroupList,
       u.forSiderealTarget,
       u.forConstraintList,
       u.forConstraintGroup,
       u.forObservationData,
       u.forProposal
      )
    )
}
