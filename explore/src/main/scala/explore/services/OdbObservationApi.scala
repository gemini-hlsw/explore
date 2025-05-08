// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.syntax.option.*
import clue.data.Input
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.ConfigurationRequestWithObsIds
import explore.model.ExecutionOffsets
import explore.model.Observation
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Group
import lucuma.core.model.ObservationReference
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.TimingWindow
import lucuma.core.util.TimeSpan
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.ObservingMode

import java.time.Instant

trait OdbObservationApi[F[_]]:
  def updateObservations(input:               UpdateObservationsInput): F[Unit]
  def updateObservations(obsIds:              List[Observation.Id], input: ObservationPropertiesInput): F[Unit]
  def updateObservationConstraintSet(
    obsIds:      List[Observation.Id],
    constraints: ConstraintSet
  ): F[Unit]
  def updateVisualizationTime(
    obsIds:          List[Observation.Id],
    observationTime: Option[Instant]
  ): F[Unit]
  def updateVisualizationDuration(
    obsIds:              List[Observation.Id],
    observationDuration: Option[TimeSpan]
  ): F[Unit]
  def updateVisualizationTimeAndDuration(
    obsIds:              List[Observation.Id],
    observationTime:     Option[Instant],
    observationDuration: Option[TimeSpan]
  ): F[Unit]
  def updatePosAngle(
    obsIds:             List[Observation.Id],
    posAngleConstraint: PosAngleConstraint
  ): F[Unit]
  def updateNotes(obsIds:                     List[Observation.Id], notes: Option[NonEmptyString]): F[Unit]
  def createObservation(programId:            Program.Id, parentId:        Option[Group.Id]): F[Observation]
  def createObservationWithTargets(programId: Program.Id, targetIds:       Set[Target.Id]): F[Observation]
  def cloneObservation(obsId:                 Observation.Id, newGroupId:  Option[Group.Id]): F[Observation]
  def applyObservation(
    obsId:           Observation.Id,
    onTargets:       Option[List[Target.Id]] = none,
    onConstraintSet: Option[ConstraintSet] = none,
    onTimingWindows: Option[List[TimingWindow]] = none
  ): F[Observation]
  def deleteObservation(obsId:                Observation.Id): F[Unit]
  def undeleteObservation(obsId:              Observation.Id): F[Unit]
  def deleteObservations(obsIds:              List[Observation.Id]): F[Unit]
  def undeleteObservations(obsIds:            List[Observation.Id]): F[Unit]

  /**
   * @param programId
   * @param obsId
   * @param groupId
   *   Group to move to. `None` to move to top level
   * @param groupIndex
   *   New index in group. `None` to leave position unchanged
   */
  def moveObservation(
    obsId:      Observation.Id,
    groupId:    Option[Group.Id],
    groupIndex: NonNegShort
  ): F[Unit]
  def setGuideTargetName(obsId:          Observation.Id, targetName: Option[NonEmptyString]): F[Unit]
  def createConfigurationRequest(
    obsId:         Observation.Id,
    justification: Option[NonEmptyString]
  ): F[ConfigurationRequestWithObsIds]
  def updateConfiguration(
    obsId:              Observation.Id,
    observingMode:      Input[ObservingModeInput],
    posAngleConstraint: Input[PosAngleConstraintInput] = Input.ignore
  ): F[Option[ObservingMode]]
  def setObservationWorkflowState(obsId: Observation.Id, st:         ObservationWorkflowState): F[Unit]
  def resolveObservationReference(
    obsRef: ObservationReference
  ): F[Option[(Program.Id, Observation.Id)]]
  def sequenceOffsets(obsId:             Observation.Id): F[Option[ExecutionOffsets]]
