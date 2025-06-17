// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import explore.model.ProgramTime
import io.circe.Decoder
import lucuma.core.math.Offset
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.model.sequence.SequenceDigest
import lucuma.core.util.CalculatedValue
import lucuma.core.util.TimeSpan
import lucuma.odb.json.sequence.given
import lucuma.schemas.decoders.given
import monocle.Focus
import monocle.Lens
import monocle.Optional

import scala.collection.immutable.SortedSet

case class Execution(
  digest:            CalculatedValue[Option[ExecutionDigest]],
  programTimeCharge: ProgramTime
) derives Eq:
  def acqOffset = digest.value.foldMap(_.acquisition.offsets)
  def sciOffset = digest.value.foldMap(_.science.offsets)

object Execution:
  val digest: Lens[Execution, CalculatedValue[Option[ExecutionDigest]]] =
    Focus[Execution](_.digest)

  val programTimeCharge: Lens[Execution, ProgramTime] =
    Focus[Execution](_.programTimeCharge)

  val sciOffset: Optional[Execution, SortedSet[Offset]] =
    digest
      .andThen(CalculatedValue.value.some)
      .andThen(ExecutionDigest.science.andThen(SequenceDigest.offsets))

  val acqOffset: Optional[Execution, SortedSet[Offset]] =
    digest
      .andThen(CalculatedValue.value.some)
      .andThen(ExecutionDigest.acquisition.andThen(SequenceDigest.offsets))

  given Decoder[Execution] = Decoder.instance(c =>
    for {
      d  <- c.get[CalculatedValue[Option[ExecutionDigest]]]("digest")
      pt <- c.get[ProgramTime]("timeCharge")
    } yield Execution(d, pt)
  )
