// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import lucuma.core.model.Target

/**
 * Parameters for the onClone parameter to the target edit.
 *
 * onClone is called when the target is cloned, and also during undo/redo.
 *
 * @param originalId
 *   The id of the target that was cloned
 * @param cloneId
 *   The id of the clone
 * @param obsIds
 *   The observation ids for which the target was cloned
 * @param areCreating
 *   True if this is set or redo, false for undo
 */
case class OnCloneParameters(
  originalId:  Target.Id,
  cloneId:     Target.Id,
  obsIds:      ObsIdSet,
  areCreating: Boolean
) derives Eq:
  val idToAdd    = if (areCreating) cloneId else originalId
  val idToRemove = if (areCreating) originalId else cloneId
