// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.MonadThrow
import cats.effect.Resource
import cats.syntax.all.*
import clue.StreamingClient
import clue.data.syntax.*
import clue.syntax.*
import eu.timepit.refined.types.numeric.NonNegShort
import explore.model.Group
import explore.model.ProgramTimeRange
import lucuma.core.model.Program
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Enums.Existence
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import org.typelevel.log4cats.Logger
import queries.common.GroupQueriesGQL.*
import queries.common.ProgramQueriesGQL.GroupEditSubscription
import queries.common.ProgramQueriesGQL.ProgramGroupsQuery
import queries.common.ProgramSummaryQueriesGQL.GroupTimeRangeQuery

trait OdbGroupApiImpl[F[_]: MonadThrow](using StreamingClient[F, ObservationDB], Logger[F]):
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

  def groupTimeRange(groupId: Group.Id): F[Option[Option[ProgramTimeRange]]] =
    GroupTimeRangeQuery[F]
      .query(groupId)
      .raiseGraphQLErrors
      .map(_.group.map(_.timeEstimateRange))

  def allProgramGroups(programId: Program.Id): F[List[Group]] =
    ProgramGroupsQuery[F]
      .query(programId)
      .raiseGraphQLErrors
      .map(_.program.toList.flatMap(_.allGroupElements.map(_.group).flattenOption))

  def programGroupsDeltaEdits(
    programId: Program.Id
  ): Resource[F, fs2.Stream[F, GroupEditSubscription.Data.GroupEdit]] =
    GroupEditSubscription
      .subscribe[F](programId.toProgramEditInput)
      .logGraphQLErrors(_ => "Error in GroupEditSubscription subscription")
      .map(_.map(_.groupEdit))
