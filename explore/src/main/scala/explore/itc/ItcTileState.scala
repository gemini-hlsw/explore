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
  asterismResults: Pot[EitherNec[ItcTargetProblem, ItcAsterismGraphResults]],
  selectedTarget:  Option[TargetAndResults]
):
  def graphResults: Option[ItcAsterismGraphResults] = asterismResults.toOption.flatMap(_.toOption)

  def asterismGraphs: Map[ItcTarget, Either[ItcQueryProblem, ItcGraphResult]] =
    graphResults.map(_.asterismGraphs).getOrElse(Map.empty)

  def targets: List[ItcTarget] =
    asterismGraphs.keys.toList

  def targetResults: List[TargetAndResults] =
    asterismGraphs.map { case (k, v) =>
      TargetAndResults(k, v)
    }.toList

  def findGraphResults(target: ItcTarget): Option[TargetAndResults] =
    asterismGraphs
      .get(target)
      .map(TargetAndResults(target, _))

  def brightestTarget: Option[TargetAndResults] =
    graphResults.flatMap(_.brightestTarget).flatMap(findGraphResults)

  def brightestOrFirst: Option[TargetAndResults] =
    brightestTarget
      .orElse(
        asterismGraphs.headOption
          .map(_.toTargetAndResults)
      )

object ItcTileState:
  def Empty: ItcTileState = ItcTileState(Pot.pending, none)

  val asterismResults = Focus[ItcTileState](_.asterismResults)

  val selectedTarget = Focus[ItcTileState](_.selectedTarget)
