// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import lucuma.core.model.Target

/**
 * Parameters for onAsterismUpdate functions, which are called when a target is added or removed
 * from an asterism, and also during undo/redo.
 *
 * @param target
 *   The id of the target that was added or removed.
 * @param obsIds
 *   The observation ids whose asterism was updated.
 * @param isAddAction
 *   Whether the original action was a add action (versus a remove action)
 * @param areAddingTarget
 *   True if the target is currently being added.
 */
case class OnAsterismUpdateParams(
  targetId:        Target.Id,
  obsIds:          ObsIdSet,
  isAddAction:     Boolean,
  areAddingTarget: Boolean
) derives Eq:
  // Whether this is a undo, versus an original action or a redo
  val isUndo = isAddAction != areAddingTarget
