// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import boopickle.DefaultBasic.*
import cats.*
import cats.data.*
import cats.effect.*
import cats.syntax.all.*
import clue.TransactionalClient
import clue.data.syntax._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosInt
import explore.model.boopickle.ItcPicklers.given
import explore.model.itc.*
import explore.model.itc.math.*
import explore.modes.GmosNorthSpectroscopyRow
import explore.modes.GmosSouthSpectroscopyRow
import explore.modes.InstrumentRow
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.NonNegDuration
import lucuma.refined.*
import org.typelevel.log4cats.Logger
import queries.common.ITCQueriesGQL.*
import queries.schemas.ITC
import queries.schemas.itc.implicits.*
import workers.*

import java.util.UUID
import scala.concurrent.duration._
//
object ITCGraphRequests {
  // Picklers for generated types not in the model.
  private given Pickler[SpectroscopyGraphITCQuery.Data.SpectroscopyGraphBeta.Ccds]   = generatePickler
  private given Pickler[SpectroscopyGraphITCQuery.Data.SpectroscopyGraphBeta.Charts] =
    generatePickler
  private given Pickler[SpectroscopyGraphITCQuery.Data.SpectroscopyGraphBeta]        = generatePickler
  private given Pickler[SpectroscopyGraphITCQuery.Data]                              = generatePickler

  private val significantFigures =
    SignificantFiguresInput(
      6.refined[Positive].assign,
      6.refined[Positive].assign,
      3.refined[Positive].assign
    ).assign

  def queryItc[F[_]: Concurrent: Parallel: Logger](
    wavelength:   Wavelength,
    exposureTime: NonNegDuration,
    exposures:    PosInt,
    constraints:  ConstraintSet,
    targets:      NonEmptyList[ItcTarget],
    mode:         InstrumentRow,
    cache:        Cache[F],
    callback:     ItcChartResult => F[Unit]
  )(using Monoid[F[Unit]], TransactionalClient[F, ITC]): F[Unit] =

    val itcRowsParams = mode match // Only handle known modes
      case m: GmosNorthSpectroscopyRow =>
        ItcGraphRequestParams(wavelength, exposureTime, exposures, constraints, targets, m).some
      case m: GmosSouthSpectroscopyRow =>
        ItcGraphRequestParams(wavelength, exposureTime, exposures, constraints, targets, m).some
      case _                           =>
        none

    def doRequest(
      request: ItcGraphRequestParams
    ): F[List[(ItcTarget, SpectroscopyGraphITCQuery.Data)]] =
      request.target
        .fproduct(t => selectedBrightness(t.profile, request.wavelength))
        .collect { case (t, Some(brightness)) =>
          request.mode.toITCInput
            .map(mode =>
              SpectroscopyGraphITCQuery
                .query(
                  SpectroscopyGraphModeInput(
                    request.wavelength.toInput,
                    request.exposureTime.toInput,
                    request.exposures,
                    t.profile.toInput,
                    brightness,
                    t.rv.toITCInput,
                    request.constraints,
                    mode,
                    significantFigures
                  ).assign
                )
                .map(r => t -> r)
            )
        }
        .flatten
        .sequence

    // We cache unexpanded results, exactly as received from server.
    val cacheableRequest = Cacheable(CacheName("itcGraphQuery"), CacheVersion(1), doRequest)

    itcRowsParams.map { request =>
      Logger[F].debug(
        s"ITC: Request for mode ${request.mode} and target count: ${request.target.length}"
      ) *>
        cache
          .eval(cacheableRequest)
          .apply(request)
          .flatMap {
            _.map { case (t, r) =>
              val charts = r.spectroscopyGraphBeta.charts.map(_.toItcChart)
              val ccds   = r.spectroscopyGraphBeta.ccds

              (ccds.toNel, charts.toNel)
                .mapN((ccds, charts) => ItcChartResult(t, ccds, charts))
                .map(callback)
                .orEmpty
            }.sequence.void
          }
    }.orEmpty

}
