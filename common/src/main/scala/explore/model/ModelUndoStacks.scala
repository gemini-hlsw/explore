// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import explore.undo.UndoStacks
import monocle.Focus

case class ModelUndoStacks[F[_]](
  forProposal: UndoStacks[F, ProgramDetails] = UndoStacks.empty[F, ProgramDetails]
)

object ModelUndoStacks:
  def forProposal[F[_]] = Focus[ModelUndoStacks[F]](_.forProposal)

  given eqModelUndoStacks[F[_]]: Eq[ModelUndoStacks[F]] =
    Eq.by(u => u.forProposal)
