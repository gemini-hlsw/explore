// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.itc

import cats.data.*
import cats.syntax.all.*
import explore.modes.*
import explore.optics.all.*
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.Timestamp
import monocle.Focus
import mouse.boolean.*

// Simple cache of the remotely calculated values
case class ItcResultsCache(
  cache: Map[ItcRequestParams, EitherNec[ItcTargetProblem, ItcResult]]
) {
  def mode(r: ModeRow): EitherNec[ItcQueryProblem, ItcInstrumentConfig] =
    Either.fromOption(
      r.enabled.option(r.instrument),
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
            _.forall: t =>
              BandNormalizedBrightnesses
                .get(t.sourceProfile)
                .exists(_.nonEmpty) ||
                EmissionLinesBrightnesses
                  .get(t.sourceProfile)
                  .exists(_.nonEmpty)
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
    etm: ExposureTimeMode,
    c:   ConstraintSet,
    a:   Option[NonEmptyList[ItcTarget]],
    l:   List[Timestamp],
    r:   ModeRow
  ): EitherNec[ItcTargetProblem, ItcResult] =
    (mode(r), targets(a)).parTupled
      .leftMap(_.map(ItcTargetProblem(none, _)))
      .map: (im, a) =>
        cache
          .get(ItcRequestParams(etm, c, a, l, im))
          .getOrElse(ItcResult.Pending.rightNec[ItcTargetProblem])
      .flatten

  def size: Int = cache.size

  def isEmpty: Boolean = size === 0

}

object ItcResultsCache:

  val Empty: ItcResultsCache = ItcResultsCache(Map.empty)

  val cache = Focus[ItcResultsCache](_.cache)
