// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.itc

import cats.data._
import cats.syntax.all._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.model.itc._
import explore.modes._
import lucuma.core.enums._
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import monocle.Focus
import mouse.boolean._

// Simple cache of the remotely calculated values
final case class ItcResultsCache(
  cache: Map[ItcRequestParams, EitherNec[ItcQueryProblems, ItcResult]]
) {
  def wavelength(w: Option[Wavelength]): EitherNec[ItcQueryProblems, Wavelength] =
    Either.fromOption(w, NonEmptyChain.of(ItcQueryProblems.MissingWavelength))

  def signalToNoise(w: Option[PosBigDecimal]): EitherNec[ItcQueryProblems, PosBigDecimal] =
    Either.fromOption(w, NonEmptyChain.of(ItcQueryProblems.MissingSignalToNoise))

  def mode(r: SpectroscopyModeRow): EitherNec[ItcQueryProblems, InstrumentRow] =
    Either.fromOption(ItcResultsCache.enabledRow(r).option(r.instrument),
                      NonEmptyChain.of(ItcQueryProblems.UnsupportedMode)
    )

  def targets(r: Option[ItcTarget]): EitherNec[ItcQueryProblems, ItcTarget] =
    Either.fromOption(r, NonEmptyChain.of(ItcQueryProblems.MissingTargetInfo))

  def update(
    newEntries: Map[ItcRequestParams, EitherNec[ItcQueryProblems, ItcResult]]
  ): ItcResultsCache =
    copy(cache ++ newEntries)

  def updateN(
    newEntries: List[Map[ItcRequestParams, EitherNec[ItcQueryProblems, ItcResult]]]
  ): ItcResultsCache =
    newEntries.foldLeft(this)(_.update(_))

  // Read the cache value or a default
  def forRow(
    w:  Option[Wavelength],
    sn: Option[PosBigDecimal],
    c:  ConstraintSet,
    t:  Option[ItcTarget],
    r:  SpectroscopyModeRow
  ): EitherNec[ItcQueryProblems, ItcResult] =
    (wavelength(w), signalToNoise(sn), mode(r), targets(t)).parMapN { (w, sn, im, t) =>
      cache
        .get(ItcRequestParams(w, sn, c, t, im))
        .getOrElse(ItcResult.Pending.rightNec[ItcQueryProblems])
    }.flatten

  def size: Int = cache.size

  def isEmpty: Boolean = size === 0

}

object ItcResultsCache {

  def enabledRow(row: SpectroscopyModeRow): Boolean =
    List[Instrument](Instrument.GmosNorth, Instrument.GmosSouth).contains_(
      row.instrument.instrument
    ) && row.focalPlane === FocalPlane.SingleSlit

  val cache = Focus[ItcResultsCache](_.cache)
}
