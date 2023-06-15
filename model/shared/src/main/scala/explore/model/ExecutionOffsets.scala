// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import io.circe.Decoder
import lucuma.core.math.Offset
import lucuma.core.model.sequence.ExecutionSequence
import lucuma.odb.json.offset.decoder.given
import lucuma.schemas.decoders.given

case class StepConfigOffset(offset: Option[Offset]) derives Decoder

case class StepOffset(stepConfig: StepConfigOffset) derives Decoder

case class AtomOffset(steps: List[StepOffset]) derives Decoder

case class ExecutionSequence(
  nextAtom:       AtomOffset,
  possibleFuture: List[AtomOffset]
) derives Decoder

case class ExecutionOffsets(science: ExecutionSequence, acquisition: ExecutionSequence)
    derives Decoder:
  def allScienceOffsets: List[Offset] =
    science.nextAtom.steps.flatMap(_.stepConfig.offset) ++
      science.possibleFuture.flatMap(_.steps.flatMap(_.stepConfig.offset))

  def allAcquisitionOffsets: List[Offset] =
    acquisition.nextAtom.steps.flatMap(_.stepConfig.offset) ++
      acquisition.possibleFuture.flatMap(_.steps.flatMap(_.stepConfig.offset))
