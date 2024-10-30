// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.itc

import cats.data.*
import cats.syntax.all.*
import explore.modes.*
import explore.optics.all.*
import lucuma.core.enums.*
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.schemas.model.CentralWavelength
import monocle.Focus
import mouse.boolean.*
// import lucuma.core.model.SourceProfile

// Simple cache of the remotely calculated values
case class ItcResultsCache(
  cache: Map[ItcRequestParams, EitherNec[ItcTargetProblem, ItcResult]]
) {
  def wavelength(
    w: Option[Wavelength],
    r: SpectroscopyModeRow
  ): EitherNec[ItcQueryProblem, CentralWavelength] =
    Either.fromOption(
      w.flatMap(r.intervalCenter),
      NonEmptyChain.of(ItcQueryProblem.MissingWavelength)
    )

  def signalToNoise(w: Option[SignalToNoise]): EitherNec[ItcQueryProblem, SignalToNoise] =
    Either.fromOption(w, NonEmptyChain.of(ItcQueryProblem.MissingSignalToNoise))

  def signalToNoiseAt(w: Option[Wavelength]): EitherNec[ItcQueryProblem, Wavelength] =
    Either.fromOption(w, NonEmptyChain.of(ItcQueryProblem.MissingSignalToNoiseAt))

  def mode(r: SpectroscopyModeRow): EitherNec[ItcQueryProblem, InstrumentConfig] =
    Either.fromOption(
      ItcResultsCache.enabledRow(r).option(r.instrument),
      NonEmptyChain.of(ItcQueryProblem.UnsupportedMode)
    )

  private def targets(
    r: Option[NonEmptyList[ItcTarget]]
  ): EitherNec[ItcQueryProblem, NonEmptyList[ItcTarget]] =
    Either
      .fromOption(r, NonEmptyChain.of(ItcQueryProblem.MissingTargetInfo))
      .flatMap: a =>
        Either.fromOption(
          a.some.filter: // All targets must have brightness defined.
            _.forall(t => SourceProfileBrightnesses.get(t.sourceProfile).exists(_.nonEmpty))
          ,
          NonEmptyChain.of(ItcQueryProblem.MissingBrightness)
        )

  def update(
    newEntries: Map[ItcRequestParams, EitherNec[ItcTargetProblem, ItcResult]]
  ): ItcResultsCache =
    copy(cache ++ newEntries)

  def updateN(
    newEntries: List[Map[ItcRequestParams, EitherNec[ItcTargetProblem, ItcResult]]]
  ): ItcResultsCache =
    newEntries.foldLeft(this)(_.update(_))

  // Read the cache value or a default
  def forRow(
    w:    Option[Wavelength],
    sn:   Option[SignalToNoise],
    snAt: Option[Wavelength],
    c:    ConstraintSet,
    a:    Option[NonEmptyList[ItcTarget]],
    r:    SpectroscopyModeRow
  ): EitherNec[ItcTargetProblem, ItcResult] =
    (wavelength(w, r), signalToNoise(sn), signalToNoiseAt(snAt), mode(r), targets(a)).parTupled
      .leftMap(_.map(ItcTargetProblem(none, _)))
      .map: (w, sn, snAt, im, a) =>
        cache
          .get(ItcRequestParams(w, sn, snAt, c, a, im))
          .getOrElse(ItcResult.Pending.rightNec[ItcTargetProblem])
      .flatten

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
