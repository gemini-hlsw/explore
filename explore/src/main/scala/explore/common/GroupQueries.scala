// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.effect.Async
import cats.syntax.all.*
import clue.FetchClient
import clue.data.syntax.*
import eu.timepit.refined.types.numeric.NonNegShort
import explore.DefaultErrorPolicy
import lucuma.core.model.Group
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.GroupPropertiesInput
import lucuma.schemas.ObservationDB.Types.UpdateGroupsInput
import lucuma.schemas.ObservationDB.Types.WhereGroup
import lucuma.schemas.ObservationDB.Types.WhereOrderGroupId
import queries.common.GroupQueriesGQL.UpdateGroupsMutation

object GroupQueries:

  /**
   * @param groupId
   *   Group to move
   * @param parentGroup
   *   New parent group, `None` to move to top level
   * @param parentGroupIndex
   *   New index in parent group, `None` to leave position unchanged
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
