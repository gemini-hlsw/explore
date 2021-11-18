// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.data._
import cats.syntax.all._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.model.ConstraintSet
import explore.modes._
import explore.schemas.itcschema.implicits._
import japgolly.scalajs.react._
import lucuma.core.enum._
import lucuma.core.math.Wavelength
import monocle.Focus

// Simple cache of the remotely calculated values
final case class ItcResultsCache(
  cache: Map[ItcResultsCache.CacheKey, EitherNec[ItcQueryProblems, ItcResult]]
) {
  import ITCRequests._

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
    c:  ConstraintSet,
    r:  SpectroscopyModeRow
  ): EitherNec[ItcQueryProblems, ItcResult] =
    (wavelength(w), signalToNoise(sn), mode(r)).parMapN { (w, sn, im) =>
      cache.get((w, sn, c, im)).getOrElse(ItcResult.Pending.rightNec[ItcQueryProblems])
    }.flatten

}

object ItcResultsCache {
  type CacheKey = (Wavelength, PosBigDecimal, ConstraintSet, InstrumentModes)

  def enabledRow(row: SpectroscopyModeRow): Boolean =
    List(Instrument.GmosNorth, Instrument.GmosSouth).contains_(
      row.instrument.instrument
    ) && row.focalPlane === FocalPlane.SingleSlit

  val cache = Focus[ItcResultsCache](_.cache)
}
