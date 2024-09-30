// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.Order.given
import cats.syntax.all.*
import crystal.Pot
import eu.timepit.refined.auto.autoUnwrap
import eu.timepit.refined.cats.*
import explore.data.tree.Node
import explore.model.GroupTree
import explore.model.GroupUpdate
import explore.model.Observation
import explore.model.ProgramSummaries
import explore.model.ServerIndexed
import explore.model.syntax.all.*
import lucuma.core.model.Group
import lucuma.schemas.ObservationDB.Enums.EditType
import lucuma.schemas.ObservationDB.Enums.EditType.*
import lucuma.schemas.ObservationDB.Enums.Existence
import queries.common.ObsQueriesGQL.ProgramObservationsDelta.Data.ObservationEdit
import queries.common.ProgramQueriesGQL.ProgramEditAttachmentSubscription.Data.ProgramEdit as AttachmentProgramEdit
import queries.common.ProgramQueriesGQL.ProgramInfoDelta.Data.ProgramEdit
import queries.common.TargetQueriesGQL.ProgramTargetsDelta.Data.TargetEdit
import cats.Monoid
import cats.MonoidK
import cats.Endo

/**
 * Functions to modify cache through subscription updates
 */
trait CacheModifierUpdaters {
  // TODO Move somewhere else
  private given [A]: Monoid[Endo[A]] = MonoidK[Endo].algebra[A]

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
    val isPresentInServer: Boolean =
      observationEdit.meta.exists(_.existence === Existence.Present)

    observationEdit.value // We ignore updates on deleted groups.
      .filter(_ => isPresentInServer || observationEdit.editType =!= EditType.Updated)
      .map: value =>
        val obsId: Observation.Id = value.id

        val obsUpdate: ProgramSummaries => ProgramSummaries =
          ProgramSummaries.observations
            .modify: observations =>
              if (isPresentInServer)
                observations.inserted(
                  obsId,
                  value,
                  observations.getIndex(obsId).getOrElse(observations.length)
                )
              else observations.removed(obsId)

        // TODO: this won't be needed anymore when groups are also updated through events of observation updates.
        val groupsUpdate: ProgramSummaries => ProgramSummaries =
          if (isPresentInServer)
            updateGroupsMappingForObsEdit(observationEdit)
          else identity

        val programTimesReset: ProgramSummaries => ProgramSummaries =
          ProgramSummaries.programTimesPot.replace(Pot.pending)

        val obsExecutionReset: ProgramSummaries => ProgramSummaries =
          ProgramSummaries.obsExecutionPots.modify: oem =>
            if (isPresentInServer) oem.withUpdatePending(obsId)
            else oem.removed(obsId)

        obsUpdate >>> groupsUpdate >>> programTimesReset >>> obsExecutionReset
      // .getOrElse(identity)
      .orEmpty
    // updateGroupsMappingForObsEdit(observationEdit)

  protected def modifyGroups(groupUpdate: GroupUpdate): ProgramSummaries => ProgramSummaries =
    val isPresentInServer: Boolean = groupUpdate.payload.exists(_.existence === Existence.Present)
    groupUpdate.payload // We ignore updates on deleted groups.
      .filter(_ => isPresentInServer || groupUpdate.editType =!= EditType.Updated)
      .map: payload =>
        val groupId: Group.Id = payload.value.elem.group.id

        val updateGroup: ProgramSummaries => ProgramSummaries =
          ProgramSummaries.groups.modify: groupTree =>

            // TODO CHECK WHAT EXACTLY HAPPENS WHEN WE GET A REFERENCE FOR A (YET) UNEXISTING OBSERVATION

            val mod: GroupTree => GroupTree =
              if (!isPresentInServer)
                _.removed(groupId.asRight)
              else
                groupUpdate.editType match
                  case DeletedCal =>
                    _.removed(groupId.asRight)
                  case _          =>
                    // The group may have changed, or its contents.
                    val newChildren: List[GroupTree.Node] =
                      payload.value.elem.elements
                        .map: indexedElem =>
                          indexedElem.elem.fold(
                            obsId =>
                              Node(ServerIndexed(obsId.asLeft, indexedElem.parentIndex), Nil),
                            groupId =>
                              val oldNode: GroupTree.Node =
                                groupTree.getNodeAndIndexByKey(groupId.asRight).get._1
                              Node(
                                ServerIndexed(oldNode.value.elem, indexedElem.parentIndex),
                                oldNode.children
                              )
                          )
                        .sortBy(_.value.parentIndex)

                    val findIndexFn: GroupTree.Node => Boolean =
                      _.value.parentIndex >= payload.value.parentIndex

                    _.updated(
                      groupId.asRight,
                      payload.value.map(_.group.asRight),
                      newChildren,
                      payload.parentGroupId.map(_.asRight),
                      findIndexFn
                    )
            mod(groupTree)

        val groupTimeRangePotsReset: ProgramSummaries => ProgramSummaries =
          ProgramSummaries.groupTimeRangePots
            .modify:
              if isPresentInServer then _.withUpdatePending(groupId)
              else _.removed(groupId)
            .andThen(parentGroupTimeRangeReset(groupId.asRight))

        updateGroup >>> groupTimeRangePotsReset
      // .getOrElse(identity)
      .orEmpty

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
    val obsId: Observation.Id = observationEdit.observationId
    (observationEdit.value, observationEdit.meta)
      .mapN: (newObservation, meta) =>

        val findIndexFn: GroupTree.Node => Boolean =
          _.value.parentIndex >= meta.groupIndex

        val editGroup: ProgramSummaries => ProgramSummaries =
          ProgramSummaries.groups
            .modify:
              _.updated(
                obsId.asLeft,
                ServerIndexed(obsId.asLeft, meta.groupIndex),
                meta.groupId.map(_.asRight),
                findIndexFn
              )

        // I'm almost sure we don't need this anymore. It happens via group edit.
        // val parentTimeRangePotsReset: ProgramSummaries => ProgramSummaries =
        //   programSummaries =>
        //     programSummaries.groups
        //       .getNodeAndIndexByKey(obsId.asLeft)
        //       // I think this may be wrong, the parent could be empty if it's the root node
        //       .flatMap(_._2.parentKey)
        //       .map(gid => parentGroupTimeRangeReset(gid))
        //       .getOrElse(identity)(programSummaries)

        editGroup // >>> parentTimeRangePotsReset
      .getOrElse:
        ProgramSummaries.groups
          .modify: groupTree =>
            if (observationEdit.editType === EditType.DeletedCal)
              groupTree.removed(obsId.asLeft)
            else groupTree

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
