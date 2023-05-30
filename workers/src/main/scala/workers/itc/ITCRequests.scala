// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import boopickle.DefaultBasic.*
import cats.*
import cats.data.*
import cats.effect.*
import cats.effect.std.Semaphore
import cats.syntax.all.*
import clue.ResponseException
import crystal.ViewF
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.DefaultErrorPolicy
import explore.model.Constants
import explore.model.Progress
import explore.model.boopickle.ItcPicklers.given
import explore.model.itc.*
import explore.model.itc.math.*
import explore.modes.GmosNorthSpectroscopyRow
import explore.modes.GmosSouthSpectroscopyRow
import explore.modes.InstrumentRow
import explore.modes.SpectroscopyModeRow
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.itc.IntegrationTime
import lucuma.itc.client.GmosFpu
import lucuma.itc.client.InstrumentMode
import lucuma.itc.client.IntegrationTimeResult
import lucuma.itc.client.ItcClient
import lucuma.itc.client.SpectroscopyIntegrationTimeInput
import org.scalajs.dom
import org.typelevel.log4cats.Logger
import queries.schemas.itc.syntax.*
import workers.*

import java.util.UUID
import scala.concurrent.duration.*

object ITCRequests:
  // Copied from https://gist.github.com/gvolpe/44e2263f9068efe298a1f30390de6d22
  def parTraverseN[F[_]: Concurrent: Parallel, G[_]: Traverse, A, B](
    n:  Long,
    ga: G[A]
  )(f: A => F[B]) =
    Semaphore[F](n).flatMap { s =>
      ga.parTraverse(a => s.permit.use(_ => f(a)))
    }

  def queryItc[F[_]: Concurrent: Parallel: Logger](
    wavelength:      Wavelength,
    signalToNoise:   SignalToNoise,
    constraints:     ConstraintSet,
    targets:         ItcTarget,
    modes:           List[SpectroscopyModeRow],
    signalToNoiseAt: Option[Wavelength],
    cache:           Cache[F],
    callback:        Map[ItcRequestParams, EitherNec[ItcQueryProblems, ItcResult]] => F[Unit]
  )(using Monoid[F[Unit]], ItcClient[F]): F[Unit] = {
    def itcResults(r: IntegrationTimeResult): EitherNec[ItcQueryProblems, ItcResult] =
      // Convert to usable types
      val i = r.result.head
      ItcResult
        .Result(i.exposureTime, i.exposures)
        .rightNec

    def doRequest(
      params: ItcRequestParams
    ): F[Option[Map[ItcRequestParams, EitherNec[ItcQueryProblems, ItcResult]]]] =
      Logger[F]
        .debug(
          s"ITC: Request for mode: ${params.mode}, centralWavelength: ${params.wavelength} and target count: ${params.target.name.value}"
        ) *> (selectedBand(params.target.profile, params.wavelength.value),
              params.mode.toItcClientMode
      )
        .traverseN { (band, mode) =>
          ItcClient[F]
            .spectroscopy(
              SpectroscopyIntegrationTimeInput(
                wavelength = params.wavelength.value,
                signalToNoise = params.signalToNoise,
                sourceProfile = params.target.profile,
                signalToNoiseAt = params.signalToNoiseAt,
                band = band,
                radialVelocity = params.target.rv,
                constraints = params.constraints,
                mode = mode
              ),
              false
            )
            .map(r => Map(params -> itcResults(r)))
            .handleError { e =>
              val msg = e match
                case ResponseException(errors, _) =>
                  errors.map(_.message).mkString_("\n")
                case e                            =>
                  e.getMessage
              Map(
                params -> (ItcQueryProblems.GenericError(msg): ItcQueryProblems).leftNec[ItcResult]
              )
            }
        }

    val cacheVersion = CacheVersion(5)

    val cacheableRequest =
      Cacheable(
        CacheName("itcQuery"),
        cacheVersion,
        doRequest,
        (r, g) =>
          g.exists(_.get(r).forall {
            case Right(_)                               => true
            case Left(ItcQueryProblems.GenericError(_)) => false
            case Left(_)                                => true
          })
      )

    val itcRowsParams = modes
      .map(x => (x.intervalCenter(wavelength), x.instrument))
      // Only handle known modes
      .collect {
        case (Some(wavelength), m: GmosNorthSpectroscopyRow) =>
          ItcRequestParams(wavelength, signalToNoise, signalToNoiseAt, constraints, targets, m)
        case (Some(wavelength), m: GmosSouthSpectroscopyRow) =>
          ItcRequestParams(wavelength, signalToNoise, signalToNoiseAt, constraints, targets, m)
      }

    parTraverseN(
      Constants.MaxConcurrentItcRequests.toLong,
      itcRowsParams.reverse
    ) { params =>
      // ITC supports sending many modes at once, but sending them one by one
      // maximizes cache hits
      cache.eval(cacheableRequest).apply(params).flatMap(_.map(callback).orEmpty)
    }.void
  }
