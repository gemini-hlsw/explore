// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.groups

import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import eu.timepit.refined.types.numeric.NonNegShort
import explore.cache.CacheModifierUpdaters
import explore.common.GroupQueries
import explore.model.GroupList
import explore.model.GroupObs
import explore.model.Grouping
import explore.undo.Action
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.schemas.ObservationDB
import queries.schemas.odb.ObsQueries

object GroupActions extends CacheModifierUpdaters:

  def moveObs(programId: Program.Id, obsId: Observation.Id)(using
    FetchClient[IO, ObservationDB]
  ): Action[GroupList, Option[GroupMoveAction]] =
    Action[GroupList, Option[GroupMoveAction]](
      getter = groups =>
        groups
          .collectFirstSome(g =>
            g.value.left.toOption.filter(_.id === obsId).tupleLeft(g.parentGroupId)
          )
          .map((parentGroupId, value) => GroupMoveAction(parentGroupId, value.groupIndex)),
      setter = action =>
        groups =>
          action.fold(groups)(action =>
            updateObservations(
              action.dropNodeId,
              GroupObs(obsId, action.dropIndex)
            )(groups)
          ),
      onSet = (a, b) =>
        b.map(action =>
          ObsQueries
            .moveObservation[IO](programId, obsId, action.dropNodeId, action.dropIndex.some)
        ).orEmpty,
    )

  private def findGrouping(groupId: Group.Id): GroupList => Option[Grouping] = _.collectFirstSome(
    _.value.toOption.filter(_.id === groupId)
  )

  def moveGroup(groupId: Group.Id)(using
    FetchClient[IO, ObservationDB]
  ): Action[GroupList, Option[GroupMoveAction]] =
    Action[GroupList, Option[GroupMoveAction]](
      getter =
        findGrouping(groupId).andThen(_.map(g => GroupMoveAction(g.parentId, g.parentIndex))),
      setter = action =>
        groups =>
          (action, findGrouping(groupId)(groups)).mapN { (action, grouping) =>
            val updatedGrouping =
              grouping.copy(parentId = action.dropNodeId, parentIndex = action.dropIndex)
            updateGroups(groupId, updatedGrouping)(groups)

          }.orEmpty,
      onSet = (_, action) =>
        action
          .map(action =>
            GroupQueries.moveGroup[IO](groupId, action.dropNodeId, action.dropIndex.some)
          )
          .orEmpty,
    )

case class GroupMoveAction(dropNodeId: Option[Group.Id], dropIndex: NonNegShort)
