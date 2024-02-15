// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.effect.Async
import cats.syntax.all.*
import clue.FetchClient
import clue.data.syntax.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import explore.DefaultErrorPolicy
import explore.model.Grouping
import lucuma.core.model.Group
import lucuma.core.model.Program
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import queries.common.GroupQueriesGQL.*

object GroupQueries:

  /**
   * @param groupId
   *   Group to move
   * @param parentGroup
   *   New parent group, `None` to move to top level
   * @param parentGroupIndex
   *   New index in parent group, `None` to leave position unchanged (or let backend decide index
   *   when moving)
   */
  def moveGroup[F[_]: Async](
    groupId:          Group.Id,
    parentGroup:      Option[Group.Id],
    parentGroupIndex: Option[NonNegShort]
  )(using FetchClient[F, ObservationDB]) =
    val input = UpdateGroupsInput(
      WHERE = WhereGroup(id = WhereOrderGroupId(EQ = groupId.assign).assign).assign,
      SET = GroupPropertiesInput(
        parentGroup = parentGroup.orUnassign,
        parentGroupIndex = parentGroupIndex.orIgnore
      )
    )
    UpdateGroupsMutation[F].execute(input).void

  def updateGroup[F[_]: Async](groupId: Group.Id, set: GroupPropertiesInput)(using
    FetchClient[F, ObservationDB]
  ): F[Unit] =
    UpdateGroupsMutation[F]
      .execute(
        UpdateGroupsInput(
          WHERE = WhereGroup(id = WhereOrderGroupId(EQ = groupId.assign).assign).assign,
          SET = set
        )
      )
      .void

  def createGroup[F[_]: Async](programId: Program.Id)(using
    FetchClient[F, ObservationDB]
  ): F[Grouping] =
    CreateGroupMutation[F]
      .execute(
        CreateGroupInput(
          programId = programId.assign
        )
      )
      .map(_.createGroup.group)
