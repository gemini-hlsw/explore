// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.itc

import cats.data.*
import cats.syntax.all.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.model.itc.*
import explore.modes.*
import lucuma.core.enums.*
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.schemas.model.CentralWavelength
import monocle.Focus
import mouse.boolean.*

// Simple cache of the remotely calculated values
case class ItcResultsCache(
  cache: Map[ItcRequestParams, EitherNec[ItcQueryProblems, ItcResult]]
) {
  def wavelength(
    w: Option[Wavelength],
    r: SpectroscopyModeRow
  ): EitherNec[ItcQueryProblems, CentralWavelength] =
    Either.fromOption(
      w.flatMap(r.intervalCenter),
      NonEmptyChain.of(ItcQueryProblems.MissingWavelength)
    )

  def signalToNoise(w: Option[PosBigDecimal]): EitherNec[ItcQueryProblems, PosBigDecimal] =
    Either.fromOption(w, NonEmptyChain.of(ItcQueryProblems.MissingSignalToNoise))

  def mode(r: SpectroscopyModeRow): EitherNec[ItcQueryProblems, InstrumentRow] =
    Either.fromOption(
      ItcResultsCache.enabledRow(r).option(r.instrument),
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
    w:    Option[Wavelength],
    sn:   Option[PosBigDecimal],
    snAt: Option[Wavelength],
    c:    ConstraintSet,
    t:    Option[ItcTarget],
    r:    SpectroscopyModeRow
  ): EitherNec[ItcQueryProblems, ItcResult] =
    (wavelength(w, r), signalToNoise(sn), snAt.validNec.toEither, mode(r), targets(t)).parMapN {
      (w, sn, snAt, im, t) =>
        cache
          .get(ItcRequestParams(w, sn, snAt, c, t, im))
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
