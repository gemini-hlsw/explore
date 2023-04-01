// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import explore.common.AsterismQueries.ProgramSummaries
import explore.undo.UndoStacks
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.core.model.Proposal
import lucuma.core.model.Target
import monocle.Focus
import queries.schemas.odb.ObsQueries.ObservationList
import queries.schemas.odb.ObsQueries.ScienceData

case class ModelUndoStacks[F[_]](
  forObsList:           UndoStacks[F, ObservationList] = UndoStacks.empty[F, ObservationList],
  forAsterismGroupList: UndoStacks[F, ProgramSummaries] = UndoStacks.empty[F, ProgramSummaries],
  forSiderealTarget:    Map[Target.Id, UndoStacks[F, Target.Sidereal]] =
    Map.empty[Target.Id, UndoStacks[F, Target.Sidereal]],
  forObservationData:   Map[Observation.Id, UndoStacks[F, ScienceData]] =
    Map.empty[Observation.Id, UndoStacks[F, ScienceData]],
  forProposal:          UndoStacks[F, Proposal] = UndoStacks.empty[F, Proposal]
)

object ModelUndoStacks:
  def forObsList[F[_]]           = Focus[ModelUndoStacks[F]](_.forObsList)
  def forAsterismGroupList[F[_]] = Focus[ModelUndoStacks[F]](_.forAsterismGroupList)
  def forSiderealTarget[F[_]]    = Focus[ModelUndoStacks[F]](_.forSiderealTarget)
  def forObservationData[F[_]]   = Focus[ModelUndoStacks[F]](_.forObservationData)
  def forProposal[F[_]]          = Focus[ModelUndoStacks[F]](_.forProposal)

  given eqModelUndoStacks[F[_]]: Eq[ModelUndoStacks[F]] =
    Eq.by(u =>
      (u.forObsList,
       u.forAsterismGroupList,
       u.forSiderealTarget,
       u.forObservationData,
       u.forProposal
      )
    )
// }
