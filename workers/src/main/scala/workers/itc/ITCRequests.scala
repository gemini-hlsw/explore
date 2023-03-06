// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import boopickle.DefaultBasic.*
import cats.*
import cats.data.*
import cats.effect.*
import cats.effect.std.Semaphore
import cats.syntax.all.*
import clue.FetchClient
import clue.data.syntax.*
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
import explore.modes.SpectroscopyModeRow
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import org.scalajs.dom
import org.typelevel.log4cats.Logger
import queries.common.ITCQueriesGQL.*
import queries.schemas.ITC
import queries.schemas.itc.ITCConversions.*
import workers.*

import java.util.UUID
import scala.concurrent.duration.*

object ITCRequests:
  // Copied from https://gist.github.com/gvolpe/44e2263f9068efe298a1f30390de6d22
  def parTraverseN[F[_]: Concurrent: Parallel, G[_]: Traverse, A, B](
    n:  Long,
    ga: G[A]
  )(f:  A => F[B]) =
    Semaphore[F](n).flatMap { s =>
      ga.parTraverse(a => s.permit.use(_ => f(a)))
    }

  def queryItc[F[_]: Concurrent: Parallel: Logger](
    wavelength:      Wavelength,
    signalToNoise:   PosBigDecimal,
    constraints:     ConstraintSet,
    targets:         ItcTarget,
    modes:           List[SpectroscopyModeRow],
    signalToNoiseAt: Option[Wavelength],
    cache:           Cache[F],
    callback:        Map[ItcRequestParams, EitherNec[ItcQueryProblems, ItcResult]] => F[Unit]
  )(using Monoid[F[Unit]], FetchClient[F, ?, ITC]): F[Unit] = {
    def itcResults(r: ItcResults): List[EitherNec[ItcQueryProblems, ItcResult]] =
      // Convert to usable types
      r.spectroscopy.flatMap(_.results).map { r =>
        r.itc match {
          case ItcError(m)      => ItcQueryProblems.GenericError(m).leftNec
          case ItcSuccess(e, t) => ItcResult.Result(t.microseconds.microseconds, e).rightNec
        }
      }

    def doRequest(
      params: ItcRequestParams
    ): F[Option[Map[ItcRequestParams, EitherNec[ItcQueryProblems, ItcResult]]]] =
      Logger[F]
        .debug(
          s"ITC: Request for mode: ${params.mode}, centralWavelength: ${params.wavelength} and target count: ${params.target.name.value}"
        ) *> selectedBand(params.target.profile, params.wavelength.value)
        .map { band =>
          SpectroscopyITCQuery[F]
            .query(
              SpectroscopyModeInput(
                wavelength = params.wavelength.value.toInput,
                signalToNoise = params.signalToNoise,
                sourceProfile = params.target.profile.toInput,
                signalToNoiseAt = params.signalToNoiseAt.map(_.toInput).orIgnore,
                band = band,
                radialVelocity = params.target.rv.toITCInput,
                constraints = params.constraints,
                modes = params.mode.toITCInput.map(_.assign).toList
              ).assign
            )
            .flatTap { r =>
              val prefix = s"ITC: Result for mode ${params.mode}:"
              itcResults(r).traverse(_ match {
                case Left(errors)                                     =>
                  Logger[F].error(s"$prefix ERRORS: $errors")
                case Right(ItcResult.Result(exposureTime, exposures)) =>
                  Logger[F].debug(s"$prefix $exposures x ${exposureTime.toSeconds}")
                case Right(other)                                     =>
                  Logger[F].debug(s"$prefix $other")
              })
            }
            .map(r =>
              itcResults(r) match {
                case Nil => none
                case l   =>
                  Map(
                    params ->
                      l.maxBy {
                        case Right(ItcResult.Result(exposureTime, _)) => exposureTime.toMicros
                        case _                                        => Long.MinValue
                      }
                  ).some
              }
            )
            .handleErrorWith(e =>
              Map(
                params -> (ItcQueryProblems
                  .GenericError("Error calling ITC service"): ItcQueryProblems)
                  .leftNec[ItcResult]
              ).some.pure[F]
            )
        }
        .sequence
        .map(_.flatten)

    val cacheableRequest = Cacheable(CacheName("itcQuery"), CacheVersion(1), doRequest)

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
