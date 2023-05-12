// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import explore.model.ProgramSummaries
import explore.undo.UndoStacks
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.core.model.Proposal
import lucuma.core.model.Target
import monocle.Focus
import queries.schemas.odb.ObsQueries.ObservationList

case class ModelUndoStacks[F[_]](
  forObsList:          UndoStacks[F, ObservationList] =
    UndoStacks.empty[F, ObservationList],                 // FIXME This is inside programsummaries!
  forProgramSummaries: UndoStacks[F, ProgramSummaries] = UndoStacks.empty[F, ProgramSummaries],
  forSiderealTarget:   Map[Target.Id, UndoStacks[F, Target.Sidereal]] =
    Map.empty[Target.Id, UndoStacks[F, Target.Sidereal]], // FIXME This is inside programsummaries!
  forProposal:         UndoStacks[F, Proposal] = UndoStacks.empty[F, Proposal]
)

object ModelUndoStacks:
  def forObsList[F[_]]          = Focus[ModelUndoStacks[F]](_.forObsList)
  def forProgramSummaries[F[_]] = Focus[ModelUndoStacks[F]](_.forProgramSummaries)
  def forSiderealTarget[F[_]]   = Focus[ModelUndoStacks[F]](_.forSiderealTarget)
  def forProposal[F[_]]         = Focus[ModelUndoStacks[F]](_.forProposal)

  given eqModelUndoStacks[F[_]]: Eq[ModelUndoStacks[F]] =
    Eq.by(u => (u.forObsList, u.forProgramSummaries, u.forSiderealTarget, u.forProposal))
