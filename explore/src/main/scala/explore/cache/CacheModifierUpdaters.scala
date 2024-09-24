// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.Order.given
import cats.syntax.all.*
import crystal.Pot
import explore.data.tree.KeyedIndexedTree.Index
import explore.data.tree.Node
import explore.model.GroupTree
import explore.model.Observation
import explore.model.ProgramSummaries
import explore.model.syntax.all.*
import lucuma.core.model.Group
import lucuma.schemas.ObservationDB.Enums.EditType
import lucuma.schemas.ObservationDB.Enums.EditType.*
import lucuma.schemas.ObservationDB.Enums.Existence
import queries.common.ObsQueriesGQL.ProgramObservationsDelta.Data.ObservationEdit
import queries.common.ProgramQueriesGQL.GroupEditSubscription.Data.GroupEdit
import queries.common.ProgramQueriesGQL.ProgramEditAttachmentSubscription.Data.ProgramEdit as AttachmentProgramEdit
import queries.common.ProgramQueriesGQL.ProgramInfoDelta.Data.ProgramEdit
import queries.common.TargetQueriesGQL.ProgramTargetsDelta.Data.TargetEdit

/**
 * Functions to modify cache through subscription updates
 */
trait CacheModifierUpdaters {

  protected def modifyTargets(targetEdit: TargetEdit): ProgramSummaries => ProgramSummaries =
    ProgramSummaries.targets
      .modify(targets =>
        if (targetEdit.meta.existence === Existence.Present)
          targets.updated(targetEdit.value.id, targetEdit.value.target)
        else
          targets.removed(targetEdit.value.id)
      )

  protected def modifyObservations(
    observationEdit: ObservationEdit
  ): ProgramSummaries => ProgramSummaries =
    observationEdit.value
      .map { value =>
        val obsId  = value.id
        val exists = observationEdit.meta.forall(_.existence === Existence.Present)

        val obsUpdate    = ProgramSummaries.observations
          .modify(observations =>
            if (exists)
              observations.inserted(
                obsId,
                value,
                observations.getIndex(obsId).getOrElse(observations.length)
              )
            else
              observations.removed(obsId)
          )
        // TODO: this won't be needed anymore when groups are also updated through events of observation updates
        val groupsUpdate = updateGroupsMappingForObsEdit(observationEdit)

        val programTimesReset = ProgramSummaries.programTimesPot.replace(Pot.pending)
        val obsExecutionReset = ProgramSummaries.obsExecutionPots.modify(oem =>
          if (exists) oem.withUpdatePending(obsId)
          else oem.removed(obsId)
        )

        obsUpdate.andThen(groupsUpdate).andThen(programTimesReset).andThen(obsExecutionReset)
      }
      .getOrElse(
        updateGroupsMappingForObsEdit(observationEdit)
      )

  protected def modifyGroups(groupEdit: GroupEdit): ProgramSummaries => ProgramSummaries =
    (groupEdit.value, groupEdit.meta).tupled
      .map: (newGrouping, meta) =>
        val groupId   = newGrouping.id
        val editType  = groupEdit.editType
        val existence = meta.existence

        val groupUpdate = ProgramSummaries.groups.modify:
          if (existence === Existence.Deleted)
            _.removed(groupId.asRight)
          else
            editType match
              case DeletedCal       =>
                _.removed(groupId.asRight)
              case Created          =>
                _.inserted(
                  groupId.asRight,
                  Node(newGrouping.toGroupTreeGroup.asRight),
                  newGrouping.toIndex
                )
              case EditType.Updated =>
                _.updated(
                  groupId.asRight,
                  newGrouping.toGroupTreeGroup.asRight,
                  newGrouping.toIndex
                )

        val groupTimeRangePotsReset = ProgramSummaries.groupTimeRangePots
          .modify(
            if existence === Existence.Present then _.withUpdatePending(groupId)
            else _.removed(groupId)
          )
          .andThen(parentGroupTimeRangeReset(groupId.asRight))

        groupUpdate.andThen(groupTimeRangePotsReset)
      .getOrElse(identity)

  protected def modifyAttachments(
    programEdit: AttachmentProgramEdit
  ): ProgramSummaries => ProgramSummaries = {
    val obsAttachments      = programEdit.value.obsAttachments.toSortedMap(_.id)
    val proposalAttachments = programEdit.value.proposalAttachments
    ProgramSummaries.obsAttachments
      .replace(obsAttachments)
      .compose(ProgramSummaries.proposalAttachments.replace(proposalAttachments))
  }

  protected def modifyPrograms(programEdit: ProgramEdit): ProgramSummaries => ProgramSummaries =
    ProgramSummaries.programs
      .modify(_.updated(programEdit.value.id, programEdit.value))

  /**
   * Update the groups for an observation edit. When an observation is updated, we also need to
   * update the groups it belongs to.
   */
  private def updateGroupsMappingForObsEdit(
    observationEdit: ObservationEdit
  ): ProgramSummaries => ProgramSummaries =
    val obsId = observationEdit.observationId
    observationEdit.value
      .map { value =>

        val groupEdit = ProgramSummaries.groups
          .modify { groupElements =>
            val groupId     = value.groupId
            val newGroupObs = GroupTree.Obs(obsId)
            val newIndex    =
              Index(groupId.map(_.asRight[Observation.Id]), value.groupIndex)

            if (observationEdit.editType === EditType.Created)
              groupElements.inserted(
                obsId.asLeft,
                Node(newGroupObs.asLeft),
                newIndex
              )
            else if (observationEdit.meta.forall(_.existence === Existence.Deleted))
              groupElements.removed(obsId.asLeft)
            else if (observationEdit.editType === EditType.Updated)
              groupElements.updated(
                obsId.asLeft,
                newGroupObs.asLeft,
                newIndex
              )
            else groupElements
          }

        val parentTimeRangePotsReset = value.groupId
          .map(gid => parentGroupTimeRangeReset(gid.asRight))
          .getOrElse(identity[ProgramSummaries])

        groupEdit.andThen(parentTimeRangePotsReset)
      }
      .getOrElse {
        ProgramSummaries.groups
          .modify { groupElements =>
            if (observationEdit.editType === EditType.DeletedCal)
              groupElements.removed(obsId.asLeft)
            else groupElements
          }
      }

  /**
   * Reset the time range pots for all parent groups of the given id
   */
  private def parentGroupTimeRangeReset(
    id: Either[Observation.Id, Group.Id]
  ): ProgramSummaries => ProgramSummaries =
    programSummaries =>
      val groupTimeRangePots = programSummaries.groups
        .parentKeys(id)
        .flatMap(_.toOption.tupleRight(Pot.pending))
        .toMap
      ProgramSummaries.groupTimeRangePots.modify(_.allUpdated(groupTimeRangePots))(
        programSummaries
      )

}
