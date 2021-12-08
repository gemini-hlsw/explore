// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.data._
import cats.syntax.all._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.model.ConstraintSet
import explore.model.ITCTarget
import explore.modes._
import japgolly.scalajs.react._
import lucuma.core.enum._
import lucuma.core.math.Wavelength
import monocle.Focus
import mouse.boolean._

// Simple cache of the remotely calculated values
final case class ItcResultsCache(
  cache: Map[ITCRequestParams, EitherNec[ItcQueryProblems, ItcResult]]
) {
  def wavelength(w: Option[Wavelength]): EitherNec[ItcQueryProblems, Wavelength] =
    Either.fromOption(w, NonEmptyChain.of(ItcQueryProblems.MissingWavelength))

  def signalToNoise(w: Option[PosBigDecimal]): EitherNec[ItcQueryProblems, PosBigDecimal] =
    Either.fromOption(w, NonEmptyChain.of(ItcQueryProblems.MissingSignalToNoise))

  def mode(r: SpectroscopyModeRow): EitherNec[ItcQueryProblems, InstrumentRow] =
    Either.fromOption(ItcResultsCache.enabledRow(r).option(r.instrument),
                      NonEmptyChain.of(ItcQueryProblems.UnsupportedMode)
    )

  def targets(r: Option[List[ITCTarget]]): EitherNec[ItcQueryProblems, NonEmptyList[ITCTarget]] =
    Either.fromOption(r.flatMap(NonEmptyList.fromList),
                      NonEmptyChain.of(ItcQueryProblems.UnsupportedMode)
    )

  // Read the cache value or a default
  def forRow(
    w:  Option[Wavelength],
    sn: Option[PosBigDecimal],
    c:  ConstraintSet,
    t:  Option[List[ITCTarget]],
    r:  SpectroscopyModeRow
  ): EitherNec[ItcQueryProblems, ItcResult] =
    (wavelength(w), signalToNoise(sn), mode(r), targets(t)).parMapN { (w, sn, im, t) =>
      cache
        .get(ITCRequestParams(w, sn, c, t, im))
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
