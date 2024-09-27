// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.Order.given
import cats.syntax.all.*
import crystal.Pot
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
import eu.timepit.refined.auto.autoUnwrap
import explore.model.RootModelViews.programSummaries

/**
 * Functions to modify cache through subscription updates
 */
trait CacheModifierUpdaters {

  protected def modifyTargets(targetEdit: TargetEdit): ProgramSummaries => ProgramSummaries =
    ProgramSummaries.targets
      .modify: targets =>
        if (targetEdit.meta.existence === Existence.Present)
          targets.updated(targetEdit.value.id, targetEdit.value.target)
        else
          targets.removed(targetEdit.value.id)

  protected def modifyObservations(
    observationEdit: ObservationEdit
  ): ProgramSummaries => ProgramSummaries =
    observationEdit.value
      .map: value =>
        val obsId: Observation.Id = value.id
        val exists: Boolean       = observationEdit.meta.forall(_.existence === Existence.Present)

        val obsUpdate: ProgramSummaries => ProgramSummaries =
          ProgramSummaries.observations
            .modify: observations =>
              if (exists)
                observations.inserted(
                  obsId,
                  value,
                  observations.getIndex(obsId).getOrElse(observations.length)
                )
              else observations.removed(obsId)

        // TODO: this won't be needed anymore when groups are also updated through events of observation updates
        val groupsUpdate: ProgramSummaries => ProgramSummaries =
          updateGroupsMappingForObsEdit(observationEdit)

        val programTimesReset: ProgramSummaries => ProgramSummaries =
          ProgramSummaries.programTimesPot.replace(Pot.pending)

        val obsExecutionReset: ProgramSummaries => ProgramSummaries =
          ProgramSummaries.obsExecutionPots.modify: oem =>
            if (exists) oem.withUpdatePending(obsId)
            else oem.removed(obsId)

        obsUpdate >>> groupsUpdate >>> programTimesReset >>> obsExecutionReset
      .getOrElse:
        updateGroupsMappingForObsEdit(observationEdit)

  protected def modifyGroups(groupEdit: GroupEdit): ProgramSummaries => ProgramSummaries =
    (groupEdit.value, groupEdit.meta).tupled
      .map: (newGrouping, meta) =>
        val groupId: Group.Id    = newGrouping.group.id
        val editType: EditType   = groupEdit.editType
        val existence: Existence = meta.existence

        val groupUpdate: ProgramSummaries => ProgramSummaries =
          ProgramSummaries.groups.modify:
            if (existence === Existence.Deleted)
              _.removed(groupId.asRight)
            else
              editType match
                case DeletedCal       =>
                  _.removed(groupId.asRight)
                case Created          =>
                  identity
                // _.inserted(
                //   groupId.asRight,
                //   Node(newGrouping.toGroupTreeGroup.asRight),
                //   newGrouping.toIndex
                // )
                case EditType.Updated =>
                  _.updated(
                    groupId.asRight,
                    newGrouping.toGroupTreeGroup.asRight,
                    newGrouping.toIndex
                  )

        // val groupTimeRangePotsReset: ProgramSummaries => ProgramSummaries =
        //   ProgramSummaries.groupTimeRangePots
        //     .modify:
        //       if existence === Existence.Present then _.withUpdatePending(groupId)
        //       else _.removed(groupId)
        //     .andThen(parentGroupTimeRangeReset(groupId.asRight))

        groupUpdate // >>> groupTimeRangePotsReset
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
      .map { newObservation =>
        // val groupEdit: ProgramSummaries => ProgramSummaries =
        //   programSummaries =>
        //     ProgramSummaries.groups
        //       .modify { groupElements =>
        //         // TODO groupIndex IS NOT OUR INDEX!!!!

        //         val groupId: Option[Group.Id]            = newObservation.groupId
        //         val newGroupObs: GroupTree.Obs           = GroupTree.Obs(obsId)
        //         val newParent: Option[groupElements.Key] = groupId.map(_.asRight[Observation.Id])
        //         // val newIndex: Index[Either[Observation.Id, Group.Id]] =
        //         //   Index(groupId.map(_.asRight[Observation.Id]), value.groupIndex)

        //         val findIndexFn: GroupTree.Node => Boolean =
        //           _.value match
        //             case Left(obs)    =>
        //               programSummaries.observations
        //                 .getValue(obs.id)
        //                 .forall(_.groupIndex >= value.groupIndex)
        //             case Right(group) => true // Groups always go first.

        //         // if (observationEdit.editType === EditType.Created)
        //         //   groupElements.updated(
        //         //     obsId.asLeft,
        //         //     newGroupObs.asLeft,
        //         //     newParent,
        //         //     findIndexFn
        //         //   )
        //         // else
        //         if (observationEdit.meta.forall(_.existence === Existence.Deleted))
        //           groupElements.removed(obsId.asLeft)
        //         // else if (observationEdit.editType === EditType.Updated)
        //         else // Created or Updated
        //           groupElements.updated(
        //             obsId.asLeft,
        //             newGroupObs.asLeft,
        //             newParent,
        //             findIndexFn
        //           )
        //         // else groupElements
        //       }(programSummaries)

        // val parentTimeRangePotsReset: ProgramSummaries => ProgramSummaries =
        //   programSummaries =>
        //     programSummaries.groups
        //       .getNodeAndIndexByKey(obsId.asLeft)
        //       // I think this may be wrong, the parent could be empty if it's the root node
        //       .flatMap(_._2.parentKey)
        //       .map(gid => parentGroupTimeRangeReset(gid))
        //       .getOrElse(identity)(programSummaries)

        // groupEdit >>> parentTimeRangePotsReset
        identity[ProgramSummaries]
      }
      .getOrElse:
        ProgramSummaries.groups
          .modify: groupElements =>
            if (observationEdit.editType === EditType.DeletedCal)
              groupElements.removed(obsId.asLeft)
            else groupElements

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
