// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.data._
import cats.syntax.all._
import cats.effect.Sync
import cats.effect.std.Dispatcher
import clue.data.syntax._
import clue.TransactionalClient
import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.common.ITCQueriesGQL._
import explore.schemas.ITC
import explore.schemas.itcschema.implicits._
import explore.modes._
import japgolly.scalajs.react._
import lucuma.core.enum._
import lucuma.core.math.Wavelength
import lucuma.core.model.SpatialProfile
import lucuma.core.model.SpectralDistribution

import explore.model.ConstraintSet
import explore.model.AirMassRange
import lucuma.core.model.Magnitude
import lucuma.core.math.MagnitudeValue
import scala.concurrent.duration._
import clue.data.Input
import monocle.Focus
import cats.Parallel

sealed trait ItcQueryProblems

object ItcQueryProblems {
  case object UnsupportedMode          extends ItcQueryProblems
  case object MissingWavelength        extends ItcQueryProblems
  case object MissingSignalToNoise     extends ItcQueryProblems
  case class GenericError(msg: String) extends ItcQueryProblems
}

sealed trait ItcResult

object ItcResult {
  case object SourceTooBright                                     extends ItcResult
  case object Pending                                             extends ItcResult
  case class Result(exposureTime: FiniteDuration, exposures: Int) extends ItcResult
}

final case class ItcResultsCache(
  cache:       Map[ItcResultsCache.CacheKey, EitherNec[ItcQueryProblems, ItcResult]],
  updateCount: Int = Int.MinValue
) {
  import ItcResultsCache._

  def wavelength(w: Option[Wavelength]): EitherNec[ItcQueryProblems, Wavelength] =
    Either.fromOption(w, NonEmptyChain.of(ItcQueryProblems.MissingWavelength))

  def signalToNoise(w: Option[PosBigDecimal]): EitherNec[ItcQueryProblems, PosBigDecimal] =
    Either.fromOption(w, NonEmptyChain.of(ItcQueryProblems.MissingSignalToNoise))

  def mode(r: SpectroscopyModeRow): EitherNec[ItcQueryProblems, InstrumentModes] =
    Either.fromOption(r.toMode, NonEmptyChain.of(ItcQueryProblems.UnsupportedMode))

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

}

trait ItcColumn {
  def queryItc[F[_]: Parallel: Dispatcher: Sync: TransactionalClient[*[_], ITC]](
    wavelength:    Wavelength,
    signalToNoise: PosBigDecimal,
    modes:         List[SpectroscopyModeRow],
    itcResults:    hooks.Hooks.UseState[ItcResultsCache]
  ) =
    modes
      .map(_.instrument)
      .collect { case m: GmosNorthSpectroscopyRow =>
        InstrumentModes(m.toGmosNITCInput)
      }
      .filterNot { case m =>
        itcResults.value.cache.contains((wavelength, signalToNoise, m))
      }
      // ITC supports sending many modes at once, but sending them one by one
      // maximizes cache hits
      .parTraverse_ { m =>
        SpectroscopyITCQuery
          .query(
            ITCSpectroscopyInput(
              wavelength.toITCInput,
              signalToNoise,
              SpatialProfile.PointSource,
              SpectralDistribution.Library(StellarLibrarySpectrum.A0I.asLeft),
              Magnitude(MagnitudeValue(20), MagnitudeBand.I, none, MagnitudeSystem.Vega).toITCInput,
              BigDecimal(0.1),
              ConstraintSet(
                ImageQuality.PointSix,
                CloudExtinction.PointFive,
                SkyBackground.Dark,
                WaterVapor.Median,
                AirMassRange(
                  AirMassRange.DefaultMin,
                  AirMassRange.DefaultMax
                )
              ),
              List(m.assign)
            ).assign
          )
          .flatMap { x =>
            val update = x.spectroscopy.flatMap(_.results).map { r =>
              val im = InstrumentModes(
                GmosNITCInput(r.mode.params.disperser,
                              r.mode.params.fpu,
                              r.mode.params.filter.orIgnore
                ).assign
              )
              val m  = r.itc match {
                case ItcError(m, _)   => ItcQueryProblems.GenericError(m).leftNec
                case ItcSuccess(e, t) => ItcResult.Result(t.microseconds.microseconds, e).rightNec
              }
              (wavelength, signalToNoise, im) -> m
            }
            itcResults
              .modState(
                ItcResultsCache.updateCount.modify(_ + 1) >>> ItcResultsCache.cache
                  .modify(_ ++ update)
              )
              .to[F]
          }
      }
      .runAsyncAndForgetCB
}
