// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import japgolly.scalajs.react.ReactCats.*
import japgolly.scalajs.react.Reusability
import lucuma.core.enums.SequenceType
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.itc.SingleSN
import lucuma.itc.TotalSN
import lucuma.schemas.odb.SequenceQueriesGQL.SequenceQuery

case class SequenceData(
  config:     InstrumentExecutionConfig,
  snPerClass: Map[SequenceType, (SingleSN, TotalSN)]
) derives Eq

object SequenceData:
  private def itcFromOdb(
    itc: SequenceQuery.Data.Observation.Itc
  ): Map[SequenceType, (SingleSN, TotalSN)] =
    val acq: Option[(SequenceType, (SingleSN, TotalSN))] =
      (itc.acquisition.selected.signalToNoiseAt.map(_.single),
       itc.acquisition.selected.signalToNoiseAt.map(_.total)
      ).mapN: (s, t) =>
        SequenceType.Acquisition -> (SingleSN(s), TotalSN(t))

    val sci: Option[(SequenceType, (SingleSN, TotalSN))] =
      (itc.science.selected.signalToNoiseAt.map(_.single),
       itc.science.selected.signalToNoiseAt.map(_.total)
      ).mapN: (s, t) =>
        SequenceType.Science -> (SingleSN(s), TotalSN(t))
    List(acq, sci).flattenOption.toMap

  def fromOdbResponse(data: SequenceQuery.Data): Option[SequenceData] =
    data.observation.flatMap: obs =>
      obs.execution.config.map: config =>
        SequenceData(
          config,
          itcFromOdb(obs.itc)
        )

  given Reusability[SequenceData] = Reusability.byEq
