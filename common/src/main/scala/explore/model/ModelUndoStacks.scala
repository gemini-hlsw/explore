// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import explore.common.ObsQueries.ConstraintSetData
import explore.common.ObsQueries.ObservationList
import explore.common.ObsQueries.ScienceRequirementsData
import explore.common.TargetObsQueries.PointingsWithObs
import explore.common.TargetQueries.TargetResult
import explore.undo.UndoStacks
import lucuma.core.model.Observation
import lucuma.core.model.Target
import monocle.macros.Lenses

@Lenses
case class ModelUndoStacks[F[_]](
  forObsList:             UndoStacks[F, ObservationList] = UndoStacks.empty[F, ObservationList],
  forTargetList:          UndoStacks[F, PointingsWithObs] = UndoStacks.empty[F, PointingsWithObs],
  forTarget:              Map[Target.Id, UndoStacks[F, TargetResult]] =
    Map.empty[Target.Id, UndoStacks[F, TargetResult]],
  forConstraintSet:       Map[Observation.Id, UndoStacks[F, ConstraintSetData]] =
    Map.empty[Observation.Id, UndoStacks[F, ConstraintSetData]],
  forScienceRequirements: Map[Observation.Id, UndoStacks[F, ScienceRequirementsData]] =
    Map.empty[Observation.Id, UndoStacks[F, ScienceRequirementsData]]
)

object ModelUndoStacks {
  implicit def eqModelUndoStacks[F[_]]: Eq[ModelUndoStacks[F]] =
    Eq.by(u =>
      (u.forObsList, u.forTargetList, u.forTarget, u.forConstraintSet, u.forScienceRequirements)
    )
}
