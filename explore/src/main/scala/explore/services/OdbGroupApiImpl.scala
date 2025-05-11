// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

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
import cats.MonadThrow

trait OdbGroupApiImpl[F[_]: MonadThrow](using FetchClient[F, ObservationDB]):
  def moveGroup(
    groupId:          Group.Id,
    parentGroup:      Option[Group.Id],
    parentGroupIndex: NonNegShort
  ): F[Unit] =
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

  def updateGroup(
    groupId: Group.Id,
    set:     GroupPropertiesInput
  ): F[Unit] =
    UpdateGroupsMutation[F]
      .execute:
        UpdateGroupsInput(
          WHERE = WhereGroup(id = WhereOrderGroupId(EQ = groupId.assign).assign).assign,
          SET = set
        )
      .raiseGraphQLErrors
      .void

  def createGroup(programId: Program.Id, parentId: Option[Group.Id]): F[Group] =
    CreateGroupMutation[F]
      .execute:
        CreateGroupInput(
          programId = programId.assign,
          SET = parentId.map(gId => GroupPropertiesInput(parentGroup = gId.assign)).orIgnore
        )
      .raiseGraphQLErrors
      .map: result =>
        result.createGroup.group

  def deleteGroup(groupId: Group.Id): F[Unit] =
    updateGroup(groupId, GroupPropertiesInput(existence = Existence.Deleted.assign))

  def undeleteGroup(groupId: Group.Id): F[Unit] =
    updateGroup(groupId, GroupPropertiesInput(existence = Existence.Present.assign))
