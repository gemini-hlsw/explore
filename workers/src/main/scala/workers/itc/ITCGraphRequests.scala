// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import boopickle.DefaultBasic.*
import cats.*
import cats.data.*
import cats.effect.*
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import explore.model.boopickle.ItcPicklers.given
import explore.model.itc.*
import explore.model.itc.math.*
import explore.modes.GmosNorthSpectroscopyRow
import explore.modes.GmosSouthSpectroscopyRow
import explore.modes.InstrumentRow
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.util.TimeSpan
import lucuma.itc.client.ItcClient
import lucuma.itc.client.OptimizedSpectroscopyGraphInput
import lucuma.itc.client.SignificantFiguresInput
import lucuma.refined.*
import lucuma.schemas.model.CentralWavelength
import org.typelevel.log4cats.Logger
import queries.schemas.itc.syntax.*
import workers.*

object ITCGraphRequests:
  private val significantFigures =
    SignificantFiguresInput(6.refined, 6.refined, 3.refined)

  def queryItc[F[_]: Concurrent: Parallel: Logger](
    wavelength:      CentralWavelength,
    exposureTime:    TimeSpan,
    exposures:       PosInt,
    signalToNoiseAt: Wavelength,
    constraints:     ConstraintSet,
    targets:         NonEmptyList[ItcTarget],
    mode:            InstrumentRow,
    cache:           Cache[F],
    callback:        Map[ItcTarget, Either[ItcQueryProblems, ItcChartResult]] => F[Unit]
  )(using Monoid[F[Unit]], ItcClient[F]): F[Unit] =

    val itcRowsParams = mode match // Only handle known modes
      case m: GmosNorthSpectroscopyRow =>
        ItcGraphRequestParams(wavelength,
                              exposureTime,
                              exposures,
                              signalToNoiseAt,
                              constraints,
                              targets,
                              m
        ).some
      case m: GmosSouthSpectroscopyRow =>
        ItcGraphRequestParams(wavelength,
                              exposureTime,
                              exposures,
                              signalToNoiseAt,
                              constraints,
                              targets,
                              m
        ).some
      case _                           =>
        none

    def doRequest(
      request: ItcGraphRequestParams
    ): F[Map[ItcTarget, Either[ItcQueryProblems, ItcChartResult]]] =
      request.target
        .traverse(t =>
          (selectedBand(t.profile, request.wavelength.value),
           request.mode.toItcClientMode(t.profile, request.constraints.imageQuality)
          )
            .traverseN { (band, mode) =>
              ItcClient[F]
                .optimizedSpectroscopyGraph(
                  OptimizedSpectroscopyGraphInput(
                    wavelength = request.wavelength.value,
                    signalToNoiseAt = request.signalToNoiseAt.some,
                    exposureTime = request.exposureTime,
                    exposures = request.exposures,
                    sourceProfile = t.profile,
                    band = band,
                    radialVelocity = t.rv,
                    constraints = request.constraints,
                    mode = mode,
                    significantFigures = significantFigures.some
                  ),
                  false
                )
                .map(chartResult =>
                  t -> ItcChartResult(
                    t,
                    ItcExposureTime(OverridenExposureTime.FromItc,
                                    request.exposureTime,
                                    request.exposures
                    ),
                    chartResult.ccds,
                    chartResult.charts,
                    chartResult.peakFinalSNRatio,
                    chartResult.atWavelengthFinalSNRatio,
                    chartResult.peakSingleSNRatio,
                    chartResult.atWavelengthSingleSNRatio
                  ).asRight
                )
                .handleError { e =>
                  t -> ITCRequests.processExtension(e).asLeft
                }
            }
        )
        .map(_.toList.flattenOption.toMap)

    // We cache unexpanded results, exactly as received from server.
    val cacheableRequest =
      Cacheable(
        CacheName("itcGraphQuery"),
        ITCRequests.cacheVersion,
        doRequest,
        (r, g) =>
          r.target.forall(t =>
            g.get(t).forall {
              case Right(_)                               => true
              case Left(ItcQueryProblems.GenericError(_)) => false
              case Left(_)                                => true
            }
          )
      )

    itcRowsParams
      .traverse { request =>
        Logger[F].debug(
          s"ITC: Request for mode ${request.mode} and target count: ${request.target.length}"
        ) *>
          cache
            .eval(cacheableRequest)
            .apply(request)
      }
      .flatMap {
        _.traverse(callback).void
      }
