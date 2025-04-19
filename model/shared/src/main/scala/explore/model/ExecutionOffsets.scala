// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import io.circe.Decoder
import lucuma.core.math.Offset
import lucuma.odb.json.offset.decoder.given

case class StepConfigOffset(offsets: List[Offset]) derives Decoder

case class ExecutionDigestOffsets(
  acquistion: Option[StepConfigOffset],
  science:    Option[StepConfigOffset]
)

object ExecutionDigestOffsets:
  given Decoder[ExecutionDigestOffsets] = c =>
    for
      acquisition <- c.downField("acquisition").as[Option[StepConfigOffset]]
      science     <- c.downField("science").as[Option[StepConfigOffset]]
    yield ExecutionDigestOffsets(acquisition, science)

case class ExecutionOffsets(digest: Option[ExecutionDigestOffsets]):
  def acquisitionOffsets: List[Offset] =
    digest.foldMap(_.acquistion.foldMap(_.offsets))

  def scienceOffsets: List[Offset] =
    digest.foldMap(_.science.foldMap(_.offsets))

object ExecutionOffsets:
  given Decoder[ExecutionOffsets] = c =>
    c.downField("digest").as[Option[ExecutionDigestOffsets]].map(ExecutionOffsets.apply)
