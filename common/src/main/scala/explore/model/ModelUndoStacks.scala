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
  forProposal: UndoStacks[F, Proposal] = UndoStacks.empty[F, Proposal]
)

object ModelUndoStacks:
  def forProposal[F[_]] = Focus[ModelUndoStacks[F]](_.forProposal)

  given eqModelUndoStacks[F[_]]: Eq[ModelUndoStacks[F]] =
    Eq.by(u => u.forProposal)
