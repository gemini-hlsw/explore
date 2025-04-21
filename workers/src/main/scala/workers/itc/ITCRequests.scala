// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import boopickle.DefaultBasic.*
import cats.*
import cats.data.*
import cats.effect.*
import cats.effect.std.Semaphore
import cats.syntax.all.*
import explore.model.Constants
import explore.model.boopickle.ItcPicklers.given
import explore.model.itc.*
import explore.modes.ItcInstrumentConfig
import explore.modes.SpectroscopyModeRow
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.Timestamp
import lucuma.itc.Error
import lucuma.itc.client.ClientCalculationResult
import lucuma.itc.client.ItcClient
import lucuma.itc.client.SpectroscopyInput
import lucuma.itc.client.SpectroscopyParameters
import org.typelevel.log4cats.Logger
import queries.schemas.itc.syntax.*
import workers.*

object ITCRequests:
  val cacheVersion = CacheVersion(18)

  val itcErrorToQueryProblems: Error => ItcQueryProblem =
    case Error.SourceTooBright(halfWell) => ItcQueryProblem.SourceTooBright(halfWell)
    case Error.General(message)          => ItcQueryProblem.GenericError(message)

  // Copied from https://gist.github.com/gvolpe/44e2263f9068efe298a1f30390de6d22
  def parTraverseN[F[_]: Concurrent: Parallel, G[_]: Traverse, A, B](
    n:  Long,
    ga: G[A]
  )(f: A => F[B]) =
    Semaphore[F](n).flatMap: s =>
      ga.parTraverse(a => s.permit.use(_ => f(a)))

  // Wrapper method to match the call in ItcServer.scala
  def queryItc[F[_]: Concurrent: Parallel: Logger](
    exposureTimeMode:    ExposureTimeMode,
    constraints:         ConstraintSet,
    asterism:            NonEmptyList[ItcTarget],
    customSedTimestamps: List[Timestamp],
    modes:               List[SpectroscopyModeRow],
    cache:               Cache[F],
    callback:            Map[ItcRequestParams, EitherNec[ItcTargetProblem, ItcResult]] => F[Unit]
  )(using Monoid[F[Unit]], ItcClient[F]): F[Unit] = {
    def itcResults(r: ClientCalculationResult): EitherNec[ItcTargetProblem, ItcResult] =
      // Convert to usable types
      r.targetTimes.partitionErrors.fold(
        errors =>
          errors
            .map: (error, idx) =>
              ItcTargetProblem(
                asterism.get(idx).map(_.name),
                itcErrorToQueryProblems(error)
              )
            .asLeft,
        times =>
          val i    = times.value.focus.times.focus
          val snAt = times.value.focus.signalToNoiseAt
          ItcResult
            .Result(i.exposureTime, i.exposureCount, r.targetTimes.brightestIndex, snAt)
            .rightNec
      )

    def doRequest(
      params: ItcRequestParams
    ): F[Option[Map[ItcRequestParams, EitherNec[ItcTargetProblem, ItcResult]]]] =
      Logger[F].info(
        s"ITC: Request for mode: ${params.mode}, exposureTimeMode: ${params.exposureTimeMode} and target count: ${params.asterism.length}"
      ) *>
        params.mode.toItcClientMode
          .traverse: mode =>
            ItcClient[F]
              .spectroscopy(
                SpectroscopyInput(
                  SpectroscopyParameters(
                    exposureTimeMode = params.exposureTimeMode,
                    constraints = params.constraints,
                    mode = mode
                  ),
                  params.asterism.map(_.gaiaFree.input)
                ),
                false
              )
              .map(r => Map(params -> itcResults(r)))

    val cacheableRequest =
      Cacheable(
        CacheName("itcQuery"),
        cacheVersion,
        doRequest,
        (r, g) =>
          g.exists:
            _.get(r).forall:
              case Right(_)                                     => true
              case Left(Chain(ItcQueryProblem.GenericError(_))) => false
              case Left(_)                                      => true
      )

    val itcRowsParams: List[ItcRequestParams] =
      modes
        .map(_.instrument)
        // Only handle known modes
        .collect:
          case m @ ItcInstrumentConfig.GmosNorthSpectroscopy(_, _, _, _) =>
            ItcRequestParams(exposureTimeMode, constraints, asterism, customSedTimestamps, m)
          case m @ ItcInstrumentConfig.GmosSouthSpectroscopy(_, _, _, _) =>
            ItcRequestParams(exposureTimeMode, constraints, asterism, customSedTimestamps, m)
          case m @ ItcInstrumentConfig.Flamingos2Spectroscopy(_, _, _)   =>
            ItcRequestParams(exposureTimeMode, constraints, asterism, customSedTimestamps, m)

    parTraverseN(
      Constants.MaxConcurrentItcRequests.toLong,
      itcRowsParams.reverse
    ) { params =>
      // ITC supports sending many modes at once, but sending them one by one
      // maximizes cache hits
      cache.eval(cacheableRequest).apply(params).flatMap(_.map(callback).orEmpty)
    }.void
  }
