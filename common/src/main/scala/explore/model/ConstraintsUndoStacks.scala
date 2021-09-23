// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.syntax.all._
import explore.common.ConstraintGroupQueries.ConstraintGroupList
import explore.undo.UndoStacks
import lucuma.core.model.Observation

import scala.collection.immutable.SortedSet

sealed trait ConstraintsUndoStacks[F[_]] {
  import ConstraintsUndoStacks._

  def listStacks: UndoStacks[F, ConstraintGroupList] = this match {
    case ConstraintGroupUndoStacks(_, _) => UndoStacks.empty[F, ConstraintGroupList]
    case ConstraintListUndoStacks(us)    => us
  }

  def groupStacks(obsIds: SortedSet[Observation.Id]): UndoStacks[F, ConstraintSet] = this match {
    case ConstraintGroupUndoStacks(ids, undoStacks) if ids === obsIds => undoStacks
    case _                                                            => UndoStacks.empty[F, ConstraintSet]
  }
}

object ConstraintsUndoStacks {
  case class ConstraintGroupUndoStacks[F[_]](
    ids:        SortedSet[Observation.Id],
    undoStacks: UndoStacks[F, ConstraintSet]
  ) extends ConstraintsUndoStacks[F]

  case class ConstraintListUndoStacks[F[_]](undoStacks: UndoStacks[F, ConstraintGroupList])
      extends ConstraintsUndoStacks[F]

  def empty[F[_]] = ConstraintListUndoStacks(UndoStacks.empty[F, ConstraintGroupList])

  implicit def eqConstraintsUndoStacks[F[_1]]: Eq[ConstraintsUndoStacks[F]] = Eq.instance {
    case (ConstraintListUndoStacks(l1), ConstraintListUndoStacks(l2))           => l1 === l2
    case (ConstraintGroupUndoStacks(i1, s1), ConstraintGroupUndoStacks(i2, s2)) =>
      i1 === i2 && s1 === s2
    case _                                                                      => false
  }
}
