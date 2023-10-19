// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.Order.given
import cats.syntax.all.*
import explore.model.GroupElement
import explore.model.GroupObs
import explore.model.Grouping
import explore.model.GroupingElement
import explore.model.ProgramSummaries
import explore.model.syntax.all.*
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.schemas.ObservationDB.Enums.EditType
import lucuma.schemas.ObservationDB.Enums.Existence
import monocle.Optional
import monocle.Traversal
import queries.common.ObsQueriesGQL.ProgramObservationsDelta.Data.ObservationEdit
import queries.common.ProgramQueriesGQL.GroupEditSubscription.Data.GroupEdit
import queries.common.ProgramQueriesGQL.ProgramEditAttachmentSubscription.Data.{
  ProgramEdit => AttachmentProgramEdit
}
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
  ): ProgramSummaries => ProgramSummaries = {
    val obsId = observationEdit.value.id

    val obsUpdate    = ProgramSummaries.observations
      .modify(observations =>
        if (observationEdit.meta.existence === Existence.Present)
          observations.inserted(
            obsId,
            observationEdit.value,
            observations.getIndex(obsId).getOrElse(observations.length)
          )
        else
          observations.removed(obsId)
      )
    val groupsUpdate = updateGroupsMappingForObsEdit(observationEdit)

    obsUpdate.andThen(groupsUpdate)
  }

  protected def modifyGroups(groupEdit: GroupEdit): ProgramSummaries => ProgramSummaries = {
    val newGrouping  = groupEdit.value
    val groupId      = newGrouping.id
    val editType     = groupEdit.editType
    val parentId     = newGrouping.parentId
    val newGrElement = GroupingElement(groupId, newGrouping.parentIndex)

    // TODO: remove groups (data not available yet)
    editType match
      case EditType.Created =>
        ProgramSummaries.groups.modify(groupElements =>
          groupElements :+ GroupElement(newGrouping.asRight, none)
        )
      case EditType.Updated =>
        ProgramSummaries.groups.andThen(Traversal.fromTraverse[List, GroupElement]).modify {
          val updateRootElements = GroupElement.grouping
            .filter(_.id === groupId)
            .replace(newGrouping)

          val updateParentGroupId = Optional
            .filter(GroupElement.grouping.exist(_.id === groupId))
            .andThen(GroupElement.parentGroupId)
            .replace(parentId)

          val updateGroupElements = GroupElement.grouping
            .modify(
              updateGroupElementsMapping(parentId, newGrElement.asRight, _.exists(_.id === groupId))
            )

          updateParentGroupId
            .andThen(updateRootElements)
            .andThen(updateGroupElements)
        }
  }

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
  ): ProgramSummaries => ProgramSummaries = {
    val obsId = observationEdit.value.id

    ProgramSummaries.groups
      .modify(groupElements =>
        val groupId     = observationEdit.value.groupId
        val newGroupObs = GroupObs(obsId, observationEdit.value.groupIndex)

        if (observationEdit.editType === EditType.Created)
          groupElements :+ GroupElement(newGroupObs.asLeft, none)
        else if (observationEdit.meta.existence === Existence.Deleted)
          // Remove the observation from all groupElements, including from the `elements` field
          groupElements.mapFilter(ge =>
            val newValue: Option[Either[GroupObs, Grouping]] = ge.value.bitraverse(
              value => if value.id === obsId then none else value.some,
              value =>
                value
                  .copy(elements = value.elements.filterNot(_.left.exists(_.id === obsId)))
                  .some
            )

            newValue.map(GroupElement.value.replace(_)(ge))
          )
        else if (observationEdit.editType === EditType.Updated)
          val groupElementsT = Traversal.fromTraverse[List, GroupElement]
          groupElementsT.modify {

            val updateParentGroupId = Optional
              .filter(GroupElement.groupObs.exist(_.id === obsId))
              .andThen(GroupElement.parentGroupId)
              .replace(observationEdit.value.groupId)

            // Update index for the changed observation
            val updateRootElements = GroupElement.groupObs
              .filter(_.id === obsId)
              .replace(newGroupObs)

            // Update 'elements' field to update/add/remove the observation from the group
            val updateGroupElements = GroupElement.grouping
              .modify(
                updateGroupElementsMapping(
                  groupId,
                  newGroupObs.asLeft,
                  _.left.exists(_.id === obsId)
                )
              )

            updateParentGroupId
              .andThen(updateRootElements)
              .andThen(updateGroupElements)
          }(groupElements)
        else groupElements
      )
  }

  /**
   * Update the elements of a grouping. When an observation or group is updated, we also need to
   * update the elements of the grouping it belongs to (or its new/old group)
   *
   * @param groupId
   *   id of the new group, or None if it is moved to the root group
   * @param newGroupElement
   *   new element to add or update
   * @param selectionF
   *   function to select the element to update
   */
  private def updateGroupElementsMapping(
    groupId:         Option[Group.Id],
    newGroupElement: Either[GroupObs, GroupingElement],
    selectionF:      Either[GroupObs, GroupingElement] => Boolean
  ): Grouping => Grouping =
    grouping =>
      (
        // Added
        if (groupId.contains(grouping.id) && !grouping.elements.exists(selectionF)) {
          Grouping.elements.modify(_ :+ newGroupElement)
        }
        // Moved to root group or different group
        else if (groupId.isEmpty || groupId.isDefined && !groupId.contains(grouping.id)) {
          Grouping.elements.modify(_.filterNot(selectionF))
        }
        // Updated (index changed)
        else if (groupId.contains(grouping.id)) {
          Grouping.elements.modify(_.map(e => if (selectionF(e)) newGroupElement else e))
        } else {
          identity[Grouping]
        }
      ) (grouping)

}