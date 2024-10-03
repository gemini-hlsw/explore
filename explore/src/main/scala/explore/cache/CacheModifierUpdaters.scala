// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.Endo
import cats.Monoid
import cats.MonoidK
import cats.Order.given
import cats.syntax.all.*
import crystal.Pot
import eu.timepit.refined.auto.autoUnwrap
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
    observationEdit.value // We ignore updates on deleted groups.
      .map: value =>
        val obsId: Observation.Id      = value.id
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

        ifPresentInServerOrLocally:
          obsUpdate >>> groupsUpdate >>> programTimesReset >>> obsExecutionReset
      .orEmpty

  protected def modifyGroups(groupUpdate: GroupUpdate): ProgramSummaries => ProgramSummaries =
    groupUpdate.payload // We ignore updates on deleted groups.
      .map: payload =>
        val groupId: Group.Id          = payload.value.elem.id
        val isPresentInServer: Boolean =
          groupUpdate.payload.exists(_.existence === Existence.Present)

        // The server sends updates for deleted groups. We want to filter those out,
        // but still process deletions for groups we already have.
        def ifPresentInServerOrLocally(
          mod: ProgramSummaries => ProgramSummaries
        ): ProgramSummaries => ProgramSummaries =
          programSummaries =>
            val isPresentLocally: Boolean = programSummaries.groups.contains(groupId.asRight)
            if isPresentInServer || isPresentLocally then mod(programSummaries)
            else programSummaries

        val groupMod: Endo[GroupTree] => Endo[ProgramSummaries] =
          if (groupUpdate.payload.exists(_.value.elem.system))
            ProgramSummaries.systemGroups.modify
          else
            ProgramSummaries.groups.modify

        val updateGroup: ProgramSummaries => ProgramSummaries =
          groupMod: groupTree =>
            val mod: GroupTree => GroupTree =
              if (!isPresentInServer)
                _.removed(groupId.asRight)
              else
                groupUpdate.editType match
                  case DeletedCal =>
                    _.removed(groupId.asRight)
                  case _          =>
                    val findIndexFn: GroupTree.Node => Boolean =
                      _.value.parentIndex >= payload.value.parentIndex

                    _.upserted(
                      groupId.asRight,
                      payload.value.map(_.asRight),
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

        ifPresentInServerOrLocally:
          updateGroup >>> groupTimeRangePotsReset
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
              _.upserted(
                obsId.asLeft,
                ServerIndexed(obsId.asLeft, meta.groupIndex),
                meta.groupId.map(_.asRight),
                findIndexFn
              )

        editGroup
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
