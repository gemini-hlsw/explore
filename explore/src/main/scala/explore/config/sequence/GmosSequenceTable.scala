// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.syntax.all.*
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.math.SignalToNoise
import lucuma.core.model.sequence.*
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.schemas.model.Visit
import lucuma.schemas.model.enums.AtomExecutionState
import lucuma.ui.sequence.*

private trait GmosSequenceTable[S, D]:
  def visits: List[Visit[D]]
  def config: ExecutionConfig[S, D]
  def snPerClass: Map[ObserveClass, SignalToNoise]

  private def futureSteps(
    obsClass:      ObserveClass,
    currentStepId: Option[Step.Id] // Will be shown in visits.
  )(sequence: ExecutionSequence[D]): List[SequenceRow.FutureStep[D]] =
    SequenceRow.FutureStep
      .fromAtoms(
        sequence.nextAtom +: (
          obsClass match // For acquisition, we ignore possibleFuture
            case ObserveClass.Science => sequence.possibleFuture
            case _                    => List.empty
        ),
        i => // Only show S/N for science or acq if FPU is None
          snPerClass
            .get(obsClass)
            .filter: _ =>
              i.observeClass match
                case a @ ObserveClass.Acquisition =>
                  i.instrumentConfig match
                    case DynamicConfig.GmosNorth(_, _, _, _, _, _, None) => true
                    case DynamicConfig.GmosSouth(_, _, _, _, _, _, None) => true
                    case _                                               => false
                case ObserveClass.Science         => true
                case _                            => false
      )
      .filterNot(futureStep => currentStepId.contains_(futureStep.stepId))

  private lazy val currentVisitData: Option[(Visit.Id, SequenceType, Option[Step.Id])] =
    // If the last atom of the last visit is Ongoing, the sequence is executing.
    visits.lastOption
      .filter:
        _.atoms.lastOption.exists:
          _.executionState === AtomExecutionState.Ongoing
      // We omit the Ongoing step from the visits.
      .map(visit =>
        (visit.id,
         visit.atoms.last.sequenceType,
         visit.atoms.lastOption
           .flatMap(_.steps.lastOption)
           .flatMap(_.generatedId)
        )
      )

  protected[sequence] lazy val currentVisitId: Option[Visit.Id]              =
    currentVisitData.map(_._1)
  protected[sequence] lazy val currentAtomSequenceType: Option[SequenceType] =
    currentVisitData.map(_._2)
  protected[sequence] lazy val currentStepId: Option[Step.Id]                =
    currentVisitData.flatMap(_._3)

  protected[sequence] lazy val scienceRows: List[SequenceRow[D]] =
    config.science
      .map(futureSteps(ObserveClass.Science, currentStepId))
      .orEmpty

  // Hide acquisition when science is executing or when sequence is complete.
  protected[sequence] lazy val isAcquisitionDisplayed: Boolean =
    !currentAtomSequenceType.contains_(SequenceType.Science) && scienceRows.nonEmpty

  protected[sequence] lazy val acquisitionRows: List[SequenceRow[D]] =
    config.acquisition // If we are executing Science, don't show any future acquisition rows.
      .filter(_ => isAcquisitionDisplayed)
      .map(futureSteps(ObserveClass.Acquisition, currentStepId))
      .orEmpty
