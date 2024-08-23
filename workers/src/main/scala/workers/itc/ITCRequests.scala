// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import explore.modes.GmosNorthSpectroscopyRow
import explore.modes.GmosSouthSpectroscopyRow
import explore.modes.InstrumentRow
import explore.modes.SpectroscopyModeRow
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.itc.Error
import lucuma.itc.client.IntegrationTimeResult
import lucuma.itc.client.ItcClient
import lucuma.itc.client.SpectroscopyIntegrationTimeInput
import lucuma.itc.client.SpectroscopyIntegrationTimeParameters
import org.typelevel.log4cats.Logger
import queries.schemas.itc.syntax.*
import workers.*

object ITCRequests:
  val cacheVersion = CacheVersion(15)

  val itcErrorToQueryProblems: Error => ItcQueryProblem =
    case Error.SourceTooBright(halfWell) => ItcQueryProblem.SourceTooBright(halfWell)
    case Error.General(message)          => ItcQueryProblem.GenericError(message)

  // val processError: Throwable => ItcQueryProblems =
  //   case e if e.getMessage.startsWith("TypeError: Failed to fetch") =>
  //     ItcQueryProblems.GenericError("ITC Server unreachable")
  //   case e                                                          =>
  //     ItcQueryProblems.GenericError(e.getMessage)

  // def processExtension(resp: Throwable): ItcQueryProblems =
  //   resp match {
  //     case ResponseException(errors, _)                               =>
  //       val extension = errors.map(_.extensions).head
  //       extension
  //         .map(c => JsonObject.fromMap(c))
  //         .flatMap(_.toJson.as[SourceTooBrightExtension].toOption) match {
  //         case Some(SourceTooBrightExtension(halfWell)) =>
  //           ItcQueryProblems.SourceTooBright(halfWell)
  //         case _                                        =>
  //           ItcQueryProblems.GenericError(errors.map(_.message).mkString_("\n"))
  //       }
  //     case e if e.getMessage.startsWith("TypeError: Failed to fetch") =>
  //       ItcQueryProblems.GenericError("ITC Server unreachable")
  //     case e                                                          =>
  //       ItcQueryProblems.GenericError(e.getMessage)
  //   }

  // Copied from https://gist.github.com/gvolpe/44e2263f9068efe298a1f30390de6d22
  def parTraverseN[F[_]: Concurrent: Parallel, G[_]: Traverse, A, B](
    n:  Long,
    ga: G[A]
  )(f: A => F[B]) =
    Semaphore[F](n).flatMap: s =>
      ga.parTraverse(a => s.permit.use(_ => f(a)))

  def queryItc[F[_]: Concurrent: Parallel: Logger](
    wavelength:      Wavelength,
    signalToNoise:   SignalToNoise,
    constraints:     ConstraintSet,
    asterism:        NonEmptyList[ItcTarget],
    modes:           List[SpectroscopyModeRow],
    signalToNoiseAt: Wavelength,
    cache:           Cache[F],
    callback:        Map[ItcRequestParams, EitherNec[ItcTargetProblem, ItcResult]] => F[Unit]
  )(using Monoid[F[Unit]], ItcClient[F]): F[Unit] = {
    def itcResults(r: IntegrationTimeResult): EitherNec[ItcTargetProblem, ItcResult] =
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
          val i = times.value.focus.times.focus
          ItcResult.Result(i.exposureTime, i.exposureCount, r.targetTimes.brightestIndex).rightNec
      )

    def doRequest(
      params: ItcRequestParams
    ): F[Option[Map[ItcRequestParams, EitherNec[ItcTargetProblem, ItcResult]]]] =
      Logger[F]
        .debug(
          s"ITC: Request for mode: ${params.mode}, centralWavelength: ${params.wavelength} and target count: ${params.asterism.length}"
        ) *> // selectedBand(params.target.sourceProfile, params.wavelength.value),
        params.mode
          .toItcClientMode(
            params.asterism.map(_.sourceProfile),
            params.constraints.imageQuality
          )
          .traverse: mode =>
            ItcClient[F]
              .spectroscopy(
                SpectroscopyIntegrationTimeInput(
                  SpectroscopyIntegrationTimeParameters(
                    wavelength = params.wavelength.value,
                    signalToNoise = params.signalToNoise,
                    signalToNoiseAt = params.signalToNoiseAt.some,
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
          g.exists(_.get(r).forall {
            case Right(_)                              => true
            case Left(ItcQueryProblem.GenericError(_)) => false
            case Left(_)                               => true
          })
      )

    val itcRowsParams: List[ItcRequestParams] =
      modes
        .map(x => (x.intervalCenter(wavelength), x.instrument))
        // Only handle known modes
        .collect:
          case (Some(wavelength), m: GmosNorthSpectroscopyRow) =>
            ItcRequestParams(wavelength, signalToNoise, signalToNoiseAt, constraints, asterism, m)
          case (Some(wavelength), m: GmosSouthSpectroscopyRow) =>
            ItcRequestParams(wavelength, signalToNoise, signalToNoiseAt, constraints, asterism, m)

    parTraverseN(
      Constants.MaxConcurrentItcRequests.toLong,
      itcRowsParams.reverse
    ) { params =>
      // ITC supports sending many modes at once, but sending them one by one
      // maximizes cache hits
      cache.eval(cacheableRequest).apply(params).flatMap(_.map(callback).orEmpty)
    }.void
  }
