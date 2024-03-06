// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import io.circe.Decoder
import io.circe.DecodingFailure
import lucuma.core.enums.Instrument
import lucuma.core.math.Offset
import lucuma.odb.json.offset.decoder.given

case class StepConfigOffset(offset: Option[Offset]) derives Decoder

case class StepOffset(stepConfig: StepConfigOffset) derives Decoder

case class AtomOffset(steps: List[StepOffset]) derives Decoder

case class ExecutionSequence(
  nextAtom:       AtomOffset,
  possibleFuture: List[AtomOffset]
) derives Decoder

case class ExecutionOffsets(science: ExecutionSequence, acquisition: ExecutionSequence):
  def allScienceOffsets: List[Offset] =
    science.nextAtom.steps.flatMap(_.stepConfig.offset) ++
      science.possibleFuture.flatMap(_.steps.flatMap(_.stepConfig.offset))

  def allAcquisitionOffsets: List[Offset] =
    acquisition.nextAtom.steps.flatMap(_.stepConfig.offset) ++
      acquisition.possibleFuture.flatMap(_.steps.flatMap(_.stepConfig.offset))

object ExecutionOffsets:
  private type OffsetTuple = (ExecutionSequence, ExecutionSequence)
  given Decoder[OffsetTuple] = Decoder.instance: c =>
    for
      science     <- c.downField("science").as[ExecutionSequence]
      acquisition <- c.downField("acquisition").as[ExecutionSequence]
    yield (science, acquisition)

  given Decoder[ExecutionOffsets] = Decoder.instance: c =>
    c.downField("instrument")
      .as[Instrument]
      .flatMap:
        case Instrument.GmosNorth => c.downField("gmosNorth").as[OffsetTuple]
        case Instrument.GmosSouth => c.downField("gmosSouth").as[OffsetTuple]
        case _                    => DecodingFailure("Only Gmos supported", c.history).asLeft
      .map: t =>
        ExecutionOffsets(t._1, t._2)
