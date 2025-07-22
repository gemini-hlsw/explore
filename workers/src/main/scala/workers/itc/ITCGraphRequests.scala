// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import boopickle.DefaultBasic.*
import cats.*
import cats.data.*
import cats.effect.*
import cats.syntax.all.*
import explore.model.boopickle.ItcPicklers.given
import explore.model.itc.*
import explore.modes.ItcInstrumentConfig
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.Timestamp
import lucuma.itc.Error
import lucuma.itc.client.ItcClient
import lucuma.itc.client.ItcConstraintsInput
import lucuma.itc.client.SignificantFiguresInput
import lucuma.itc.client.SpectroscopyIntegrationTimeAndGraphsInput
import lucuma.itc.client.SpectroscopyIntegrationTimeAndGraphsParameters
import lucuma.itc.client.SpectroscopyIntegrationTimeAndGraphsResult
import lucuma.refined.*
import org.typelevel.log4cats.Logger
import queries.schemas.itc.syntax.*
import workers.*

import scala.scalajs.js.JavaScriptException

object ITCGraphRequests:
  private val significantFigures =
    SignificantFiguresInput(6.refined, 6.refined, 3.refined)

  // Wrapper method to match the call in ItcServer.scala
  def queryItc[F[_]: Concurrent: Logger: ItcClient](
    exposureTimeMode:    ExposureTimeMode,
    constraints:         ConstraintSet,
    targets:             NonEmptyList[ItcTarget],
    customSedTimestamps: List[Timestamp],
    mode:                ItcInstrumentConfig,
    cache:               Cache[F],
    callback:            EitherNec[ItcQueryProblem, ItcAsterismGraphResults] => F[Unit]
  ): F[Unit] =

    val itcRowsParams = mode match // Only handle known modes
      case m @ ItcInstrumentConfig.GmosNorthSpectroscopy(_, _, _, _) =>
        ItcGraphRequestParams(
          exposureTimeMode,
          constraints,
          targets,
          customSedTimestamps,
          m
        ).some
      case m @ ItcInstrumentConfig.GmosSouthSpectroscopy(_, _, _, _) =>
        ItcGraphRequestParams(
          exposureTimeMode,
          constraints,
          targets,
          customSedTimestamps,
          m
        ).some
      case m: ItcInstrumentConfig.Flamingos2Spectroscopy             =>
        ItcGraphRequestParams(
          exposureTimeMode,
          constraints,
          targets,
          customSedTimestamps,
          m
        ).some
      case _                                                         =>
        none

    def doRequest(
      request: ItcGraphRequestParams
    ): F[EitherNec[ItcQueryProblem, ItcAsterismGraphResults]] =
      request.mode.toItcClientMode
        .map: mode =>
          ItcClient[F]
            .spectroscopyIntegrationTimeAndGraphs(
              SpectroscopyIntegrationTimeAndGraphsInput(
                SpectroscopyIntegrationTimeAndGraphsParameters(
                  exposureTimeMode = request.exposureTimeMode,
                  constraints = ItcConstraintsInput.fromConstraintSet(request.constraints),
                  mode = mode,
                  significantFigures = significantFigures.some
                ),
                request.asterism.map(_.gaiaFree.input)
              ),
              useCache = false
            )
            .map: (graphsResult: SpectroscopyIntegrationTimeAndGraphsResult) =>
              val asterismGraphs =
                graphsResult.graphsOrTimes.value
                  .fold[NonEmptyList[(ItcTarget, Either[ItcQueryProblem, ItcGraphResult])]](
                    justTimes =>
                      NonEmptyList.fromListUnsafe:
                        justTimes.value.toNonEmptyList
                          .zip(request.asterism)
                          .toList
                          .flatMap: (targetResult, itcTarget) =>
                            targetResult.value.left.toOption.map:
                              case Error.SourceTooBright(wellHalfFilledSeconds) =>
                                itcTarget -> ItcQueryProblem
                                  .SourceTooBright(wellHalfFilledSeconds)
                                  .asLeft
                              case Error.General(message)                       =>
                                itcTarget -> ItcQueryProblem.GenericError(message).asLeft
                    ,
                    _.value.toNonEmptyList
                      .zip(request.asterism)
                      .map: (targetResult, itcTarget) =>
                        itcTarget ->
                          targetResult.value.bimap(
                            ITCRequests.itcErrorToQueryProblems(_),
                            timeAndGraphs => ItcGraphResult(itcTarget, timeAndGraphs)
                          )
                  )
                  .toList
                  .toMap

              ItcAsterismGraphResults(
                asterismGraphs,
                graphsResult.brightestIndex.flatMap(request.asterism.get),
                ExposureTimeMode.at.get(request.exposureTimeMode)
              ).rightNec
            .handleError {
              case JavaScriptException(a) if a.toString.startsWith("TypeError") =>
                // This happens when the server is unreachable
                ItcQueryProblem.GenericError("ITC Server Unreachable").leftNec
              case a                                                            =>
                ItcQueryProblem.GenericError(a.getMessage).leftNec
            }
        .getOrElse(ItcQueryProblem.UnsupportedMode.leftNec.pure[F])

    // We cache unexpanded results, exactly as received from server.
    val cacheableRequest
      : Cacheable[F, ItcGraphRequestParams, EitherNec[ItcQueryProblem, ItcAsterismGraphResults]] =
      Cacheable(
        CacheName("itcGraphQuery"),
        ITCRequests.cacheVersion,
        doRequest,
        (r, e) =>
          e match
            case Left(_)  => false
            case Right(g) =>
              r.asterism.forall: t =>
                g.asterismGraphs
                  .get(t)
                  .forall:
                    case Right(_)                              => true
                    case Left(ItcQueryProblem.GenericError(_)) => false
                    case Left(_)                               => true
      )

    itcRowsParams
      .traverse: request =>
        Logger[F].debug(
          s"ITC: Request for mode ${request.mode} and target count: ${request.asterism.length}"
        ) *>
          cache
            .eval(cacheableRequest)
            .apply(request)
      .flatMap:
        _.traverse(callback).void
