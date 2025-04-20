// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.effect.Async
import cats.syntax.all.*
import clue.FetchClient
import clue.data.syntax.*
import clue.syntax.*
import eu.timepit.refined.types.numeric.NonNegShort
import explore.model.Group
import lucuma.core.model.Program
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Enums.Existence
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
    parentGroupIndex: NonNegShort
  )(using FetchClient[F, ObservationDB]) =
    UpdateGroupsMutation[F]
      .execute:
        UpdateGroupsInput(
          WHERE = WhereGroup(id = WhereOrderGroupId(EQ = groupId.assign).assign).assign,
          SET = GroupPropertiesInput(
            parentGroup = parentGroup.orUnassign,
            parentGroupIndex = parentGroupIndex.assign
          )
        )
      .raiseGraphQLErrors
      .void

  def updateGroup[F[_]: Async](
    groupId: Group.Id,
    set:     GroupPropertiesInput
  )(using
    FetchClient[F, ObservationDB]
  ): F[Unit] =
    UpdateGroupsMutation[F]
      .execute:
        UpdateGroupsInput(
          WHERE = WhereGroup(id = WhereOrderGroupId(EQ = groupId.assign).assign).assign,
          SET = set
        )
      .raiseGraphQLErrors
      .void

  def createGroup[F[_]: Async](programId: Program.Id, parentId: Option[Group.Id])(using
    FetchClient[F, ObservationDB]
  ): F[Group] =
    CreateGroupMutation[F]
      .execute:
        CreateGroupInput(
          programId = programId.assign,
          SET = parentId.map(gId => GroupPropertiesInput(parentGroup = gId.assign)).orIgnore
        )
      .raiseGraphQLErrors
      .map: result =>
        result.createGroup.group

  def deleteGroup[F[_]: Async](groupId: Group.Id)(using FetchClient[F, ObservationDB]): F[Unit] =
    updateGroup(groupId, GroupPropertiesInput(existence = Existence.Deleted.assign))

  def undeleteGroup[F[_]: Async](groupId: Group.Id)(using FetchClient[F, ObservationDB]): F[Unit] =
    updateGroup(
      groupId,
      GroupPropertiesInput(existence = Existence.Present.assign)
    )
