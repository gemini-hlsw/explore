// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats._
import cats.effect._
import cats.effect.std.Semaphore
import cats.syntax.all._
import clue.TransactionalClient
import clue.data.Input
import clue.data.syntax._
import crystal.ViewF
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.common.ITCQueriesGQL._
import explore.model.Constants
import explore.model.ConstraintSet
import explore.modes.GmosNorthSpectroscopyRow
import explore.modes.SpectroscopyModeRow
import explore.schemas.ITC
import explore.schemas.itcschema.implicits._
import japgolly.scalajs.react._
import lucuma.core.enum.StellarLibrarySpectrum
import lucuma.core.enum._
import lucuma.core.math.MagnitudeValue
import lucuma.core.math.Wavelength
import lucuma.core.model.Magnitude
import lucuma.core.model.SpatialProfile
import lucuma.core.model.SpectralDistribution
import org.typelevel.log4cats.Logger

import scala.concurrent.duration._

object ITCRequests {
  // Copied from https://gist.github.com/gvolpe/44e2263f9068efe298a1f30390de6d22
  def parTraverseN[F[_]: Concurrent: Parallel, G[_]: Traverse, A, B](
    n:  Long,
    ga: G[A]
  )(f:  A => F[B]) =
    Semaphore[F](n).flatMap { s =>
      ga.parTraverse(a => s.permit.use(_ => f(a)))
    }

  implicit class Row2Modes(val r: SpectroscopyModeRow) extends AnyVal {
    def toMode: Option[InstrumentModes] = r.instrument match {
      case GmosNorthSpectroscopyRow(d, f, fi) =>
        (new InstrumentModes(
          new GmosNITCInput(d, f, Input.orIgnore(fi)).assign
        )).some
      case _                                  => none
    }
  }

  def queryItc[F[_]: Concurrent: Parallel: Logger: TransactionalClient[*[_], ITC]](
    wavelength:    Wavelength,
    signalToNoise: PosBigDecimal,
    constraints:   ConstraintSet,
    modes:         List[SpectroscopyModeRow],
    cache:         ViewF[F, ItcResultsCache]
  ): F[Unit] =
    parTraverseN(
      Constants.ConcurrentItcRequests.toLong,
      modes
        .map(_.instrument)
        // Only handle known modes
        .collect { case m: GmosNorthSpectroscopyRow =>
          InstrumentModes(m.toGmosNITCInput)
        }
        // Discard values in the cache
        .filterNot { case m =>
          cache.get.cache.contains((wavelength, signalToNoise, constraints, m))
        }
    )(m =>
      // ITC supports sending many modes at once, but sending them one by one
      // maximizes cache hits
      doRequest(
        m,
        wavelength,
        signalToNoise,
        constraints,
        { x =>
          // Convert to usable types and update the cache
          val update = x.spectroscopy.flatMap(_.results).map { r =>
            val im = InstrumentModes(
              GmosNITCInput(r.mode.params.disperser,
                            r.mode.params.fpu,
                            r.mode.params.filter.orIgnore
              ).assign
            )
            val m  = r.itc match {
              case ItcError(m)      => ItcQueryProblems.GenericError(m).leftNec
              case ItcSuccess(e, t) => ItcResult.Result(t.microseconds.microseconds, e).rightNec
            }
            (wavelength, signalToNoise, constraints, im) -> m
          }
          cache.mod(ItcResultsCache.cache.modify(_ ++ update))
        }
      )
    ).void

  private def doRequest[F[_]: FlatMap: Logger: TransactionalClient[*[_], ITC]](
    mode:          InstrumentModes,
    wavelength:    Wavelength,
    signalToNoise: PosBigDecimal,
    constraints:   ConstraintSet,
    callback:      SpectroscopyITCQuery.Data => F[Unit]
  ): F[Unit] =
    Logger[F].info(s"ITC request for mode $mode") *>
      SpectroscopyITCQuery
        .query(
          ITCSpectroscopyInput(
            wavelength.toITCInput,
            signalToNoise,
            // TODO Link target info to explore
            SpatialProfile.PointSource,
            SpectralDistribution.Library(StellarLibrarySpectrum.A0I.asLeft),
            Magnitude(MagnitudeValue(20), MagnitudeBand.I, none, MagnitudeSystem.Vega).toITCInput,
            BigDecimal(0.1),
            constraints,
            List(mode.assign)
          ).assign
        )
        .flatMap(callback)
}
