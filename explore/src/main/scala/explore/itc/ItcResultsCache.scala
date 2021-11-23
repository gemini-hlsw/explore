// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.Parallel
import cats.data._
import cats.syntax.all._
import clue.data.Input
import clue.data.syntax._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.modes._
import explore.schemas.itcschema.implicits._
import japgolly.scalajs.react._
import lucuma.core.enum._
import lucuma.core.math.Wavelength
import monocle.Focus

import scala.concurrent.duration._

// Simple cache of the remotely calculated values
final case class ItcResultsCache(
  cache:       Map[ItcResultsCache.CacheKey, EitherNec[ItcQueryProblems, ItcResult]],
  updateCount: Int = Int.MinValue // monotonically increased to calculate reusability
) {
  import ItcResultsCache._

  def wavelength(w: Option[Wavelength]): EitherNec[ItcQueryProblems, Wavelength] =
    Either.fromOption(w, NonEmptyChain.of(ItcQueryProblems.MissingWavelength))

  def signalToNoise(w: Option[PosBigDecimal]): EitherNec[ItcQueryProblems, PosBigDecimal] =
    Either.fromOption(w, NonEmptyChain.of(ItcQueryProblems.MissingSignalToNoise))

  def mode(r: SpectroscopyModeRow): EitherNec[ItcQueryProblems, InstrumentModes] =
    Either.fromOption(r.toMode.filter(_ => ItcResultsCache.enabledRow(r)),
                      NonEmptyChain.of(ItcQueryProblems.UnsupportedMode)
    )

  // Read the cache value or a default
  def forRow(
    w:  Option[Wavelength],
    sn: Option[PosBigDecimal],
    r:  SpectroscopyModeRow
  ): EitherNec[ItcQueryProblems, ItcResult] =
    (wavelength(w), signalToNoise(sn), mode(r)).parMapN { (w, sn, im) =>
      cache.get((w, sn, im)).getOrElse(ItcResult.Pending.rightNec[ItcQueryProblems])
    }.flatten

}

object ItcResultsCache {
  type CacheKey = (Wavelength, PosBigDecimal, InstrumentModes)

  def enabledRow(row: SpectroscopyModeRow): Boolean =
    List(Instrument.GmosNorth, Instrument.GmosSouth).contains_(
      row.instrument.instrument
    ) && row.focalPlane === FocalPlane.SingleSlit

  implicit class Row2Modes(val r: SpectroscopyModeRow) extends AnyVal {
    def toMode: Option[InstrumentModes] = r.instrument match {
      case GmosNorthSpectroscopyRow(d, f, fi) =>
        (new InstrumentModes(
          new GmosNITCInput(d, f, Input.orIgnore(fi)).assign
        )).some
      case _                                  => none
    }
  }

  val cache = Focus[ItcResultsCache](_.cache)

  val updateCount = Focus[ItcResultsCache](_.updateCount)

  def queryItc[F[_]: Parallel](
    wavelength:    Wavelength,
    signalToNoise: PosBigDecimal,
    modes:         List[SpectroscopyModeRow],
    cache:         ItcResultsCache,
    cacheUpdate:   (ItcResultsCache => ItcResultsCache) => F[Unit],
    queue:         ITCRequestsQueue[F]
  ): F[Unit] =
    modes
      .map(_.instrument)
      // Only handle known modes
      .collect { case m: GmosNorthSpectroscopyRow =>
        InstrumentModes(m.toGmosNITCInput)
      }
      // Discard values in the cache
      .filterNot { case m =>
        cache.cache.contains((wavelength, signalToNoise, m))
      }
      // ITC supports sending many modes at once, but sending them one by one
      // maximizes cache hits
      .parTraverse_ { m =>
        queue.add(
          ITCRequest[F](
            m,
            wavelength,
            signalToNoise,
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
                (wavelength, signalToNoise, im) -> m
              }
              cacheUpdate(
                ItcResultsCache.updateCount.modify(_ + 1) >>> ItcResultsCache.cache
                  .modify(_ ++ update)
              )
            }
          )
        )
      }
}
