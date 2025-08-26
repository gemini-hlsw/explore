// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import boopickle.DefaultBasic.*
import cats.Eq
import cats.data.EitherNec
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.derived.*
import cats.effect.IO
import cats.syntax.all.*
import explore.events.ItcMessage
import explore.model.Constants
import explore.model.Observation
import explore.model.TargetList
import explore.model.WorkerClients.ItcClient
import explore.model.boopickle.ItcPicklers.given
import explore.model.itc.*
import explore.model.reusability.given
import explore.modes.ItcInstrumentConfig
import japgolly.scalajs.react.ReactCats.*
import japgolly.scalajs.react.Reusability
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.Timestamp
import queries.schemas.itc.syntax.*
import workers.WorkerClient

case class ItcImagingQuerier(
  observation:         Observation,
  selectedConfigs:     List[ItcInstrumentConfig],
  allTargets:          TargetList,
  customSedTimestamps: List[Timestamp]
) derives Eq:

  private val constraints = observation.constraints
  private val asterismIds = observation.scienceTargetIds

  private val exposureTimeMode: Option[ExposureTimeMode] =
    observation.scienceRequirements.exposureTimeMode

  private val itcTargets: EitherNec[ItcTargetProblem, NonEmptyList[ItcTarget]] =
    asterismIds.toItcTargets(allTargets)

  private val queryProps: EitherNec[ItcQueryProblem, ItcImagingQuerier.QueryProps] =
    for {
      t       <- itcTargets.leftMap(_.map(_.problem))
      exp     <- exposureTimeMode.toRightNec(ItcQueryProblem.MissingExposureTimeMode)
      configs <- NonEmptyList
                   .fromList(selectedConfigs)
                   .toRightNec(ItcQueryProblem.GenericError(Constants.MissingMode))
    } yield ItcImagingQuerier.QueryProps(exp, constraints, t, configs.toList, customSedTimestamps)

  def requestCalculations(using
    WorkerClient[IO, ItcMessage.Request]
  ): IO[ImagingResults] =
    def action(
      qp: ItcImagingQuerier.QueryProps
    ): IO[ImagingResults] =
      ItcClient[IO]
        .requestSingle:
          ItcMessage.Query(qp.exposureTimeMode,
                           qp.constraints,
                           qp.targets,
                           qp.customSedTimestamps,
                           qp.instrumentConfigs
          )
        .map(_.fold(ItcQueryProblem.GenericError("No response from ITC server").leftNec)(_.asRight))

    (for {
      qp <- EitherT(queryProps.pure[IO])
      r  <- EitherT(action(qp))
    } yield r).value

object ItcImagingQuerier:
  private case class QueryProps(
    exposureTimeMode:    ExposureTimeMode,
    constraints:         ConstraintSet,
    targets:             NonEmptyList[ItcTarget],
    instrumentConfigs:   List[ItcInstrumentConfig],
    customSedTimestamps: List[Timestamp]
  ) derives Eq

  private given Reusability[QueryProps] = Reusability.byEq
  given Reusability[ItcImagingQuerier]  =
    Reusability.by(_.queryProps)
