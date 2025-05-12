// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import eu.timepit.refined.types.numeric.NonNegShort
import explore.model.Group
import lucuma.core.model.Program
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*

trait OdbGroupApi[F[_]]:
  /**
   * @param groupId
   *   Group to move
   * @param parentGroup
   *   New parent group, `None` to move to top level
   * @param parentGroupIndex
   *   New index in parent group, `None` to leave position unchanged (or let backend decide index
   *   when moving)
   */
  def moveGroup(
    groupId:          Group.Id,
    parentGroup:      Option[Group.Id],
    parentGroupIndex: NonNegShort
  ): F[Unit]
  def updateGroup(groupId:   Group.Id, set:        GroupPropertiesInput): F[Unit]
  def createGroup(programId: Program.Id, parentId: Option[Group.Id]): F[Group]
  def deleteGroup(groupId:   Group.Id): F[Unit]
  def undeleteGroup(groupId: Group.Id): F[Unit]
