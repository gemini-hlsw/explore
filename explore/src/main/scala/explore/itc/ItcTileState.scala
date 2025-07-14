// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.Eq
import cats.data.EitherNec
import cats.derived.*
import cats.syntax.all.*
import crystal.*
import explore.model.itc.*
import monocle.Focus

case class TargetAndResults(
  target: ItcTarget,
  result: Either[ItcQueryProblem, ItcGraphResult]
) derives Eq:
  def asTargetProblem: EitherNec[ItcTargetProblem, ItcGraphResult] =
    result.leftMap(p => ItcTargetProblem(target.name.some, p)).toEitherNec

// we need to share this across all the ITC tiles
case class ItcTileState(
  asterismResults:       Pot[EitherNec[ItcTargetProblem, ItcAsterismGraphResults]],
  calculationResults:    Pot[
    EitherNec[ItcTargetProblem, Map[ItcRequestParams, EitherNec[ItcTargetProblem, ItcResult]]]
  ],
  selectedTarget:        Option[TargetAndResults],
  selectedImagingTarget: Option[ItcTarget] = None
):
  def graphResults: Option[ItcAsterismGraphResults] = asterismResults.toOption.flatMap(_.toOption)

  def asterismGraphs: Map[ItcTarget, Either[ItcQueryProblem, ItcGraphResult]] =
    graphResults.map(_.asterismGraphs).getOrElse(Map.empty)

  def graphsTargets: List[ItcTarget] =
    asterismGraphs.keys.toList

  def targetResults: List[TargetAndResults] =
    asterismGraphs.map { case (k, v) =>
      TargetAndResults(k, v)
    }.toList

  def findGraphResults(target: ItcTarget): Option[TargetAndResults] =
    asterismGraphs
      .get(target)
      .map(TargetAndResults(target, _))

  private def graphBrightestTarget: Option[TargetAndResults] =
    graphResults.flatMap(_.brightestTarget).flatMap(findGraphResults)

  def graphsBrightestOrFirst: Option[TargetAndResults] =
    graphBrightestTarget
      .orElse(
        asterismGraphs.headOption
          .map(_.toTargetAndResults)
      )

  // TODO: Find the brightest target for imaging, I think it depends on the filter too
  def imagingBrightest: Option[ItcTarget] =
    // For now, just pick the first target
    calculationTargets.headOption

  def calculationTargets: List[ItcTarget] =
    calculationResults.toOption
      .flatMap(_.toOption.map(_.keys.flatMap(_.asterism.toList).toList.distinct))
      .getOrElse(List.empty)

object ItcTileState:
  def Empty: ItcTileState = ItcTileState(Pot.pending, Pot.pending, none, none)

  val asterismResults = Focus[ItcTileState](_.asterismResults)

  val selectedTarget = Focus[ItcTileState](_.selectedTarget)

  val calculationResults = Focus[ItcTileState](_.calculationResults)

  val selectedImagingTarget = Focus[ItcTileState](_.selectedImagingTarget)
