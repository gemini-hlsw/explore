// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.Order.given
import cats.syntax.all.*
import crystal.Pot
import explore.givens.given
import explore.model.GroupUpdate
import explore.model.Observation
import explore.model.ProgramSummaries
import explore.model.syntax.all.*
import lucuma.core.model.Group
import lucuma.schemas.ObservationDB.Enums.EditType
import lucuma.schemas.ObservationDB.Enums.EditType.*
import lucuma.schemas.ObservationDB.Enums.Existence
import queries.common.ObsQueriesGQL.ProgramObservationsDelta.Data.ObservationEdit
import queries.common.ProgramQueriesGQL.ConfigurationRequestSubscription.Data.ConfigurationRequestEdit
import queries.common.ProgramQueriesGQL.ProgramEditAttachmentSubscription.Data.ProgramEdit as AttachmentProgramEdit
import queries.common.ProgramQueriesGQL.ProgramInfoDelta.Data.ProgramEdit
import queries.common.TargetQueriesGQL.ProgramTargetsDelta.Data.TargetEdit
import explore.model.GroupList
import explore.model.ProgramTimeRange

/**
 * Functions to modify cache through subscription updates
 */
trait CacheModifierUpdaters {
  protected def modifyTargets(targetEdit: TargetEdit): ProgramSummaries => ProgramSummaries =
    ProgramSummaries.targets
      .modify: targets =>
        if (targetEdit.meta.exists(_.existence === Existence.Present))
          targetEdit.value
            .map(t => targets.updated(targetEdit.targetId, t.target))
            .getOrElse(targets.removed(targetEdit.targetId))
        else targets.removed(targetEdit.targetId)

  protected def modifyObservations(
    observationEdit: ObservationEdit
  ): ProgramSummaries => ProgramSummaries =
    val obsId: Observation.Id = observationEdit.observationId
    observationEdit.value // We ignore updates on deleted groups.
      .map: value =>
        val isPresentInServer: Boolean =
          observationEdit.meta.exists(_.existence === Existence.Present)

        // The server sends updates for deleted observations. We want to filter those out,
        // but still process deletions for observations we already have.
        def ifPresentInServerOrLocally(
          mod: ProgramSummaries => ProgramSummaries
        ): ProgramSummaries => ProgramSummaries =
          programSummaries =>
            val isPresentLocally: Boolean = programSummaries.observations.contains(obsId)
            if isPresentInServer || isPresentLocally then mod(programSummaries)
            else programSummaries

        val obsUpdate: ProgramSummaries => ProgramSummaries =
          ProgramSummaries.observations
            .modify: observations =>
              if (isPresentInServer)
                observations + (obsId -> value)
              // observations.inserted(
              //   obsId,
              //   value,
              //   observations.getIndex(obsId).getOrElse(observations.length)
              // )
              else observations.removed(obsId)

        // // TODO: this won't be needed anymore when groups are also updated through events of observation updates.
        // val groupsUpdate: ProgramSummaries => ProgramSummaries =
        //   if (isPresentInServer)
        //     updateGroupsMappingForObsEdit(observationEdit)
        //   else identity

        val programTimesReset: ProgramSummaries => ProgramSummaries =
          ProgramSummaries.programTimesPot.replace(Pot.pending)

        val obsExecutionReset: ProgramSummaries => ProgramSummaries =
          ProgramSummaries.obsExecutionPots.modify: oem =>
            if (isPresentInServer) oem.withUpdatePending(obsId)
            else oem.removed(obsId)

        ifPresentInServerOrLocally:
          obsUpdate >>> /* groupsUpdate >>>*/ programTimesReset >>> obsExecutionReset
      .getOrElse {
        identity
        // if (observationEdit.editType === DeletedCal)
        //   ProgramSummaries.groups
        //     .modify: groupList =>
        //       groupList.removed(observationEdit.observationId.asLeft)
        // else identity
      }

  protected def modifyGroups(groupUpdate: GroupUpdate): ProgramSummaries => ProgramSummaries =
    groupUpdate.payload // We ignore updates on deleted groups.
      // 24 October 2024 - scalafix failing to parse with fewer braces
      .map { payload =>
        val groupId: Group.Id          = payload.value.elem.id
        val isPresentInServer: Boolean =
          groupUpdate.payload.exists(_.existence === Existence.Present)

        // The server sends updates for deleted groups. We want to filter those out,
        // but still process deletions for groups we already have.
        def ifPresentInServerOrLocally(
          mod: ProgramSummaries => ProgramSummaries
        ): ProgramSummaries => ProgramSummaries =
          programSummaries =>
            val isPresentLocally: Boolean = programSummaries.groups.contains(groupId)
            if isPresentInServer || isPresentLocally then mod(programSummaries)
            else programSummaries

        // val groupMod: Endo[GroupTree] => Endo[ProgramSummaries] =
        // if (groupUpdate.payload.exists(_.value.elem.system))
        //   ProgramSummaries.systemGroups.modify
        // else
        // ProgramSummaries.groups.modify

        val updateGroup: ProgramSummaries => ProgramSummaries =
          ProgramSummaries.groups.modify: groupList =>
            val mod: GroupList => GroupList =
              if (!isPresentInServer)
                _.removed(groupId)
              else
                groupUpdate.editType match
                  case DeletedCal =>
                    _.removed(groupId)
                  case _          =>
                    _ + (groupId -> payload.value.elem)
                  // _.inserted(groupId, payload.value.elem)
                  // val findIndexFn: GroupTree.Node => Boolean =
                  //   _.value.parentIndex >= payload.value.parentIndex
                  // _.upserted(
                  //   groupId.asRight,
                  //   payload.value.map(_.asRight),
                  //   payload.parentGroupId.map(_.asRight),
                  //   findIndexFn
                  // )
            mod(groupList)

        val groupTimeRangePotsReset: ProgramSummaries => ProgramSummaries =
          ProgramSummaries.groupTimeRangePots
            .modify:
              if isPresentInServer then _.withUpdatePending(groupId)
              else _.removed(groupId)
            .andThen(parentGroupTimeRangeReset(groupId.asRight))

        ifPresentInServerOrLocally:
          updateGroup >>> groupTimeRangePotsReset
      }.orEmpty

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

  protected def modifyConfigurationRequests(
    crEdit: ConfigurationRequestEdit
  ): ProgramSummaries => ProgramSummaries =
    crEdit.configurationRequest.foldMap: cr =>
      ProgramSummaries.configurationRequests
        .modify: crs =>
          crs.updated(cr.id, cr)

  // /**
  //  * Update the groups for an observation edit. When an observation is updated, we also need to
  //  * update the groups it belongs to.
  //  */
  // private def updateGroupsMappingForObsEdit(
  //   observationEdit: ObservationEdit
  // ): ProgramSummaries => ProgramSummaries =
  //   val obsId: Observation.Id = observationEdit.observationId
  //   (observationEdit.value, observationEdit.meta)
  //     .mapN: (newObservation, meta) =>

  //       val findIndexFn: GroupTree.Node => Boolean =
  //         _.value.parentIndex >= meta.groupIndex

  //       val isInSystemGroup: ProgramSummaries => Boolean = meta.groupId match {
  //         case Some(g) =>
  //           ProgramSummaries.systemGroups.exist(_.contains(g.asRight))
  //         case None    => _ => false
  //       }

  //       val groupMod: Endo[GroupTree] => Endo[ProgramSummaries] =
  //         modGroup =>
  //           programSummaries =>
  //             if (isInSystemGroup(programSummaries))
  //               ProgramSummaries.systemGroups.modify(modGroup)(programSummaries)
  //             else
  //               ProgramSummaries.groups.modify(modGroup)(programSummaries)

  //       groupMod:
  //         _.upserted(
  //           obsId.asLeft,
  //           ServerIndexed(obsId.asLeft, meta.groupIndex),
  //           meta.groupId.map(_.asRight),
  //           findIndexFn
  //         )
  //     .getOrElse:
  //       ProgramSummaries.systemGroups
  //         .modify: groupTree =>
  //           if (observationEdit.editType === EditType.DeletedCal)
  //             groupTree.removed(obsId.asLeft)
  //           else groupTree

  /**
   * Reset the time range pots for all parent groups of the given id
   */
  private def parentGroupTimeRangeReset(
    id: Either[Observation.Id, Group.Id]
  ): ProgramSummaries => ProgramSummaries =
    programSummaries =>
      val groupTimeRangePots: Map[Group.Id, Pot[Option[ProgramTimeRange]]] =
        programSummaries
          .parentGroups(id)
          .map(_ -> Pot.pending)
          .toMap
      ProgramSummaries.groupTimeRangePots.modify(_.allUpdated(groupTimeRangePots))(
        programSummaries
      )
}
