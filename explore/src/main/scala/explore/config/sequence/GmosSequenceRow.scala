// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.syntax.all.*
import lucuma.core.enums.StepType
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.NonNegDuration
import lucuma.core.model.sequence.*
import lucuma.schemas.model.StepRecord
import lucuma.schemas.model.Visit

import java.time.Duration
import java.time.Instant

sealed trait GmosSequenceRow:
  def id: String
  protected def instrumentConfig: Option[DynamicConfig]
  protected def stepConfig: Option[StepConfig]

  private def componentToArcSec[A]: Offset.Component[A] => BigDecimal =
    ((c: Offset.Component[A]) => c.toAngle)
      .andThen(Angle.signedDecimalArcseconds.get)
      .andThen(_.setScale(1, BigDecimal.RoundingMode.HALF_UP))

  lazy val stepType: Option[StepType]                       = stepConfig.map(_.stepType)
  lazy val exposureSecs: Option[Long]                       =
    instrumentConfig.map(_ match
      case DynamicConfig.GmosNorth(exposure, _, _, _, _, _, _) => exposure.getSeconds
      case DynamicConfig.GmosSouth(exposure, _, _, _, _, _, _) => exposure.getSeconds
    )
  // TODO Not in model yet, we are just simulating
  lazy val guided: Boolean                                  =
    stepConfig match
      case Some(StepConfig.Science(_)) => true
      case _                           => false
  lazy val (p, q): (Option[BigDecimal], Option[BigDecimal]) =
    stepConfig match
      case Some(StepConfig.Science(Offset(p, q))) =>
        (p, q).bimap(componentToArcSec, componentToArcSec).bimap(_.some, _.some)
      case Some(_)                                => (BigDecimal(0).some, BigDecimal(0).some)
      case _                                      => (none, none)
  lazy val wavelength: Option[BigDecimal]                   =
    instrumentConfig
      .flatMap(_ match
        case DynamicConfig.GmosNorth(_, _, _, _, grating, _, _) => grating.map(_.wavelength)
        case DynamicConfig.GmosSouth(_, _, _, _, grating, _, _) => grating.map(_.wavelength)
      )
      .map(Wavelength.decimalNanometers.reverseGet)
  lazy val gratingName: Option[String]                      =
    instrumentConfig.flatMap(_ match
      case DynamicConfig.GmosNorth(_, _, _, _, grating, _, _) =>
        grating.map(_.grating.shortName)
      case DynamicConfig.GmosSouth(_, _, _, _, grating, _, _) =>
        grating.map(_.grating.shortName)
    )
  lazy val fpuName: Option[String]                          =
    instrumentConfig.flatMap(_ match
      case DynamicConfig.GmosNorth(_, _, _, _, _, _, Some(GmosFpuMask.Builtin(fpu))) =>
        fpu.shortName.some
      case DynamicConfig.GmosSouth(_, _, _, _, _, _, Some(GmosFpuMask.Builtin(fpu))) =>
        fpu.shortName.some
      case _                                                                         =>
        none
    )
  lazy val filterName: Option[String]                       =
    instrumentConfig.flatMap(_ match
      case DynamicConfig.GmosNorth(_, _, _, _, _, filter, _) => filter.map(_.shortName)
      case DynamicConfig.GmosSouth(_, _, _, _, _, filter, _) => filter.map(_.shortName)
    )
  lazy val readoutXBin: Option[String]                      =
    instrumentConfig.flatMap(_ match
      case DynamicConfig.GmosNorth(_, readout, _, _, _, _, _) => readout.xBin.shortName.some
      case DynamicConfig.GmosSouth(_, readout, _, _, _, _, _) => readout.xBin.shortName.some
    )
  lazy val readoutYBin: Option[String]                      =
    instrumentConfig.flatMap(_ match
      case DynamicConfig.GmosNorth(_, readout, _, _, _, _, _) => readout.yBin.shortName.some
      case DynamicConfig.GmosSouth(_, readout, _, _, _, _, _) => readout.yBin.shortName.some
    )
  lazy val roi: Option[String]                              =
    instrumentConfig.flatMap(_ match
      case DynamicConfig.GmosNorth(_, _, _, roi, _, _, _) => roi.shortName.some
      case DynamicConfig.GmosSouth(_, _, _, roi, _, _, _) => roi.shortName.some
    )

object GmosSequenceRow:

  case class FutureStep(
    stepId:                     Step.Id,
    futureStepInstrumentConfig: DynamicConfig,
    futureStepConfig:           StepConfig,
    atomId:                     Atom.Id,
    firstOf:                    Option[Int]
  ) extends GmosSequenceRow:
    override lazy val id: String                              = stepId.toString
    override lazy val instrumentConfig: Option[DynamicConfig] = futureStepInstrumentConfig.some
    override lazy val stepConfig: Option[StepConfig]          = futureStepConfig.some

  object FutureStep:
    def fromStep(step: Step, atomId: Atom.Id, firstOf: Option[Int]): GmosSequenceRow.FutureStep =
      FutureStep(step.id, step.instrumentConfig, step.stepConfig, atomId, firstOf)

  sealed trait Executed extends GmosSequenceRow:
    def created: Instant
    def startTime: Option[Instant]
    def endTime: Option[Instant]
    def duration: Option[NonNegDuration]

    lazy val durationSecs: Option[Long] = duration.map(_.value.getSeconds)

  object Executed:
    case class ExecutedVisit(protected val visit: Visit) extends Executed:
      lazy val id: String                                        = visit.id.toString
      protected lazy val instrumentConfig: Option[DynamicConfig] = none
      protected lazy val stepConfig: Option[StepConfig]          = none

      export visit.{created, duration, endTime, startTime}

    case class ExecutedStep(protected val stepRecord: StepRecord) extends Executed:
      lazy val id: String                                        = stepRecord.id.toString
      protected lazy val instrumentConfig: Option[DynamicConfig] = stepRecord.instrumentConfig.some
      protected lazy val stepConfig: Option[StepConfig]          = stepRecord.stepConfig.some

      export stepRecord.{
        created,
        datasetEvents,
        datasets,
        duration,
        endTime,
        startTime,
        stepEvents,
        stepQaState
      }
