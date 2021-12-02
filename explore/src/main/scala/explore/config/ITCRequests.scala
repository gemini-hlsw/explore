// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats._
import cats.data._
import cats.effect._
import cats.effect.std.Semaphore
import cats.syntax.all._
import clue.TransactionalClient
import clue.data.syntax._
import crystal.ViewF
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.common.ITCQueriesGQL._
import explore.model.Constants
import explore.model.ConstraintSet
import explore.model.ITCTarget
import explore.model.Progress
import explore.modes.GmosNorthSpectroscopyRow
import explore.modes.InstrumentRow
import explore.modes.SpectroscopyModeRow
import explore.schemas.ITC
import explore.schemas.itcschema.implicits._
import japgolly.scalajs.react._
import lucuma.core.enum.MagnitudeBand
import lucuma.core.enum.StellarLibrarySpectrum
import lucuma.core.math.Wavelength
import lucuma.core.model.Magnitude
import lucuma.core.model.SpatialProfile
import lucuma.core.model.SpectralDistribution
import org.typelevel.log4cats.Logger

import scala.collection.immutable.SortedMap
import scala.concurrent.duration._

final case class ITCRequestParams(
  wavelength:    Wavelength,
  signalToNoise: PosBigDecimal,
  constraints:   ConstraintSet,
  target:        NonEmptyList[ITCTarget],
  mode:          InstrumentRow
)

object ITCRequests {
  // Copied from https://gist.github.com/gvolpe/44e2263f9068efe298a1f30390de6d22
  def parTraverseN[F[_]: Concurrent: Parallel, G[_]: Traverse, A, B](
    n:  Long,
    ga: G[A]
  )(f:  A => F[B]) =
    Semaphore[F](n).flatMap { s =>
      ga.parTraverse(a => s.permit.use(_ => f(a)))
    }

  def queryItc[F[_]: Concurrent: Parallel: Logger: TransactionalClient[*[_], ITC]](
    wavelength:    Wavelength,
    signalToNoise: PosBigDecimal,
    constraints:   ConstraintSet,
    targets:       NonEmptyList[ITCTarget],
    modes:         List[SpectroscopyModeRow],
    cache:         ViewF[F, ItcResultsCache],
    progress:      ViewF[F, Option[Progress]]
  ): F[Unit] = {
    val itcRowsParams = modes
      .map(_.instrument)
      // Only handle known modes
      .collect { case m: GmosNorthSpectroscopyRow =>
        ITCRequestParams(wavelength, signalToNoise, constraints, targets, m)
      }
      // Discard values in the cache
      .filterNot { case params =>
        cache.get.cache.contains(params)
      }

    progress.set(Progress.initial(NonNegInt.unsafeFrom(itcRowsParams.length)).some) >>
      parTraverseN(
        Constants.MaxConcurrentItcRequests.toLong,
        itcRowsParams
      ) { params =>
        // ITC supports sending many modes at once, but sending them one by one
        // maximizes cache hits
        doRequest(
          params,
          { x =>
            // Convert to usable types and update the cache
            val update: EitherNec[ItcQueryProblems, ItcResult] = x.toList
              .flatMap(x =>
                x.spectroscopy.flatMap(_.results).map { r =>
                  val (t, m) = r.itc match {
                    case ItcError(m)      => (Long.MinValue, ItcQueryProblems.GenericError(m).leftNec)
                    case ItcSuccess(e, t) =>
                      (t.microseconds, ItcResult.Result(t.microseconds.microseconds, e).rightNec)
                  }
                  (t, m)
                }
              )
              // There maybe multiple targets, take the one with the max time
              .maxBy(_._1)
              ._2
            cache.mod(ItcResultsCache.cache.modify(_ + (params -> update)))
          }
        ) >> progress.mod(_.map(_.increment()))
      } >> progress.set(none)
  }

  // Find the magnitude closest to the requested wavelength
  def selectedMagnitude(
    mags:       SortedMap[MagnitudeBand, Magnitude],
    wavelength: Wavelength
  ): Option[Magnitude] =
    mags.minimumByOption(b =>
      (b.band.center.toPicometers.value.value - wavelength.toPicometers.value.value).abs
    )

  private def doRequest[F[_]: Monad: Logger: TransactionalClient[*[_], ITC]](
    request:  ITCRequestParams,
    callback: List[ItcResults] => F[Unit]
  ): F[Unit] =
    Logger[F].debug(s"ITC request for mode ${request.mode}") *>
      request.target
        .fproduct(t => selectedMagnitude(t.magnitudes, request.wavelength))
        .collect { case (t, Some(m)) =>
          SpectroscopyITCQuery
            .query(
              ITCSpectroscopyInput(
                request.wavelength.toITCInput,
                request.signalToNoise,
                // TODO Link sp and SED info to explore
                SpatialProfile.PointSource,
                SpectralDistribution.Library(StellarLibrarySpectrum.A0I.asLeft),
                m.toITCInput,
                t.rv.toITCInput,
                request.constraints,
                request.mode.toITCInput.map(_.assign).toList
              ).assign
            )
        }
        .sequence
        .flatMap(callback)
}
