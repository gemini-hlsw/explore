// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import explore.common.ObsQueries.ObservationList
import explore.common.ObsQueries.ScienceData
import explore.common.TargetObsQueries.PointingsWithObs
import explore.common.TargetQueries.TargetResult
import explore.undo.UndoStacks
import lucuma.core.model.Observation
import lucuma.core.model.Target
import monocle.Focus

case class ModelUndoStacks[F[_]](
  forObsList:     UndoStacks[F, ObservationList] = UndoStacks.empty[F, ObservationList],
  forTargetList:  UndoStacks[F, PointingsWithObs] = UndoStacks.empty[F, PointingsWithObs],
  forTarget:      Map[Target.Id, UndoStacks[F, TargetResult]] =
    Map.empty[Target.Id, UndoStacks[F, TargetResult]],
  forConstraints: ConstraintsUndoStacks[F] = ConstraintsUndoStacks.empty[F],
  forScienceData: Map[Observation.Id, UndoStacks[F, ScienceData]] =
    Map.empty[Observation.Id, UndoStacks[F, ScienceData]]
)

object ModelUndoStacks {
  def forObsList[F[_]]     = Focus[ModelUndoStacks[F]](_.forObsList)
  def forTargetList[F[_]]  = Focus[ModelUndoStacks[F]](_.forTargetList)
  def forTarget[F[_]]      = Focus[ModelUndoStacks[F]](_.forTarget)
  def forConstraints[F[_]] = Focus[ModelUndoStacks[F]](_.forConstraints)
  def forScienceData[F[_]] = Focus[ModelUndoStacks[F]](_.forScienceData)

  implicit def eqModelUndoStacks[F[_]]: Eq[ModelUndoStacks[F]] =
    Eq.by(u => (u.forObsList, u.forTargetList, u.forTarget, u.forConstraints, u.forScienceData))
}
