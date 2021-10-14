// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import explore.common.ConstraintGroupQueries.ConstraintGroupList
import explore.common.ObsQueries.ObservationList
import explore.common.ObsQueries.ScienceData
import explore.common.TargetListGroupQueries.TargetListGroupList
import explore.model.ScienceTarget
import explore.undo.UndoStacks
import lucuma.core.model.Observation
import lucuma.core.model.SiderealTarget
import monocle.Focus

import scala.collection.immutable.SortedSet

case class ModelUndoStacks[F[_]](
  forObsList:         UndoStacks[F, ObservationList] = UndoStacks.empty[F, ObservationList],
  forTargetListList:  UndoStacks[F, TargetListGroupList] = UndoStacks.empty[F, TargetListGroupList],
  // forTargetList:        UndoStacks[F, PointingsWithObs] = UndoStacks.empty[F, PointingsWithObs],
  forSiderealTarget:  Map[ScienceTarget.Id, UndoStacks[F, SiderealTarget]] =
    Map.empty[ScienceTarget.Id, UndoStacks[F, SiderealTarget]],
  forConstraintList:  UndoStacks[F, ConstraintGroupList] = UndoStacks.empty[F, ConstraintGroupList],
  forConstraintGroup: Map[SortedSet[Observation.Id], UndoStacks[F, ConstraintSet]] =
    Map.empty[SortedSet[Observation.Id], UndoStacks[F, ConstraintSet]],
  forScienceData:     Map[Observation.Id, UndoStacks[F, ScienceData]] =
    Map.empty[Observation.Id, UndoStacks[F, ScienceData]]
)

object ModelUndoStacks {
  def forObsList[F[_]]         = Focus[ModelUndoStacks[F]](_.forObsList)
  def forTargetListList[F[_]]  = Focus[ModelUndoStacks[F]](_.forTargetListList)
  def forSiderealTarget[F[_]]  = Focus[ModelUndoStacks[F]](_.forSiderealTarget)
  def forConstraintList[F[_]]  = Focus[ModelUndoStacks[F]](_.forConstraintList)
  def forConstraintGroup[F[_]] = Focus[ModelUndoStacks[F]](_.forConstraintGroup)
  def forScienceData[F[_]]     = Focus[ModelUndoStacks[F]](_.forScienceData)

  implicit def eqModelUndoStacks[F[_]]: Eq[ModelUndoStacks[F]] =
    Eq.by(u =>
      (u.forObsList,
       u.forTargetListList,
       u.forSiderealTarget,
       u.forConstraintList,
       u.forConstraintGroup,
       u.forScienceData
      )
    )
}
