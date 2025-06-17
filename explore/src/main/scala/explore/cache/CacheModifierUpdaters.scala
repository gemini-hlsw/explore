// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.Order.given
import cats.syntax.all.*
import explore.givens.given
import explore.model.Attachment
import explore.model.ConfigurationRequestWithObsIds
import explore.model.Execution
import explore.model.GroupList
import explore.model.Observation
import explore.model.ProgramInfo
import explore.model.ProgramSummaries
import explore.model.syntax.all.*
import lucuma.core.model.Group
import lucuma.schemas.ObservationDB.Enums.EditType
import lucuma.schemas.ObservationDB.Enums.EditType.*
import lucuma.schemas.ObservationDB.Enums.Existence
import lucuma.ui.optics.*
import queries.common.ObsQueriesGQL.ObsCalcSubscription.Data.ObscalcUpdate
import queries.common.ObsQueriesGQL.ProgramObservationsDelta.Data.ObservationEdit
import queries.common.ProgramQueriesGQL.GroupEditSubscription.Data.GroupEdit
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

        ifPresentInServerOrLocally(obsUpdate)
      .getOrElse:
        if (observationEdit.editType === EditType.DeletedCal)
          ProgramSummaries.observations.modify(_ - obsId)
        else identity

  protected def modifyObservationCalculatedValues(
    obscalcUpdate: ObscalcUpdate
  ): ProgramSummaries => ProgramSummaries =
    val digestLens   = Observation.execution.andThen(Execution.digest)
    val disjointLens = digestLens.disjointZip(Observation.workflow)
    // `execution.calculatedDigest` and `workflow` will be made non-optional in the future.
    // In practice, both should be defined.
    obscalcUpdate.value
      .flatMap(value => (value.execution.calculatedDigest, value.calculatedWorkflow).tupled)
      .fold(identity[ProgramSummaries]) { case (digest, workflow) =>
        val obsId: Observation.Id = obscalcUpdate.observationId
        ProgramSummaries.observations
          .index(obsId)
          .andThen(disjointLens)
          .replace((digest, workflow))
      }

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

        ifPresentInServerOrLocally(updateGroup)
      }.orEmpty

  protected def modifyAttachments(
    attachments: List[Attachment]
  ): ProgramSummaries => ProgramSummaries =
    ProgramSummaries.attachments.replace(attachments.toSortedMap(_.id))

  protected def modifyPrograms(programInfo: ProgramInfo): ProgramSummaries => ProgramSummaries =
    ProgramSummaries.programs.modify(_.updated(programInfo.id, programInfo))

  protected def modifyConfigurationRequests(
    cr: ConfigurationRequestWithObsIds
  ): ProgramSummaries => ProgramSummaries =
    ProgramSummaries.configurationRequests.modify: crs =>
      crs.updated(cr.id, cr)

}
