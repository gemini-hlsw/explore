// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.Order.given
import cats.syntax.all.*
import crystal.Pot
import explore.givens.given
import explore.model.GroupList
import explore.model.Observation
import explore.model.ProgramSummaries
import explore.model.ProgramTimeRange
import explore.model.syntax.all.*
import lucuma.core.model.Group
import lucuma.schemas.ObservationDB.Enums.EditType
import lucuma.schemas.ObservationDB.Enums.EditType.*
import lucuma.schemas.ObservationDB.Enums.Existence
import queries.common.ObsQueriesGQL.ProgramObservationsDelta.Data.ObservationEdit
import queries.common.ProgramQueriesGQL.ConfigurationRequestSubscription.Data.ConfigurationRequestEdit
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
              else
                observations - obsId

        val programTimesReset: ProgramSummaries => ProgramSummaries =
          ProgramSummaries.programTimesPot.replace(Pot.pending)

        val obsExecutionReset: ProgramSummaries => ProgramSummaries =
          ProgramSummaries.obsExecutionPots.modify: oem =>
            if (isPresentInServer) oem.withUpdatePending(obsId)
            else oem.removed(obsId)

        ifPresentInServerOrLocally:
          obsUpdate >>> programTimesReset >>> obsExecutionReset
      .orEmpty

  protected def modifyGroups(groupEdit: GroupEdit): ProgramSummaries => ProgramSummaries =
    groupEdit.value // We ignore updates on deleted groups.
      // 24 October 2024 - scalafix failing to parse with fewer braces
      .map { group =>
        val groupId: Group.Id          = group.id
        val isPresentInServer: Boolean =
          groupEdit.meta.exists(_.existence === Existence.Present)

        // The server sends updates for deleted groups. We want to filter those out,
        // but still process deletions for groups we already have.
        def ifPresentInServerOrLocally(
          mod: ProgramSummaries => ProgramSummaries
        ): ProgramSummaries => ProgramSummaries =
          programSummaries =>
            val isPresentLocally: Boolean = programSummaries.groups.contains(groupId)
            if isPresentInServer || isPresentLocally then mod(programSummaries)
            else programSummaries

        val updateGroup: ProgramSummaries => ProgramSummaries =
          ProgramSummaries.groups.modify: groupList =>
            val mod: GroupList => GroupList =
              if (!isPresentInServer)
                _.removed(groupId)
              else
                groupEdit.editType match
                  case DeletedCal => _ - groupId
                  case _          => _ + (groupId -> group)
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
