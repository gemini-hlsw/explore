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

case class ImagingTargetAndResults(
  target: ItcTarget,
  result: Option[EitherNec[ItcTargetProblem, ItcResult]]
) derives Eq

// we need to share this across all the ITC tiles
case class ItcTileState(
  asterismResults:       Pot[EitherNec[ItcTargetProblem, ItcAsterismGraphResults]],
  calculationResults:    Pot[
    EitherNec[ItcTargetProblem, Map[ItcRequestParams, EitherNec[ItcTargetProblem, ItcResult]]]
  ],
  selectedTarget:        Option[TargetAndResults],
  selectedImagingTarget: Option[ImagingTargetAndResults] = None
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

  def calculationTargets: List[ItcTarget] =
    calculationResults.toOption
      .flatMap(_.toOption.map(_.keys.flatMap(_.asterism.toList).toList.distinct))
      .getOrElse(List.empty)

  def imagingTargetResults: List[ImagingTargetAndResults] =
    calculationResults.toOption.flatMap(_.toOption) match
      case Some(results) =>
        calculationTargets.map { target =>
          // For imaging, we want any successful result for this target across all configurations
          val targetResults = results
            .collectFirst {
              case (params, result) if params.asterism.toList.contains(target) && result.isRight =>
                result
            }
            .orElse {
              // If no successful results, take any result (including errors) for this target
              results.collectFirst {
                case (params, result) if params.asterism.toList.contains(target) => result
              }
            }
          ImagingTargetAndResults(target, targetResults)
        }
      case None          => List.empty

  def findImagingResults(target: ItcTarget): Option[ImagingTargetAndResults] =
    calculationResults.toOption.flatMap(_.toOption).map { results =>
      // For imaging, we want any successful result for this target across all configurations
      val targetResults = results
        .collectFirst {
          case (params, result) if params.asterism.toList.contains(target) && result.isRight =>
            result
        }
        .orElse {
          // If no successful results, take any result (including errors) for this target
          results.collectFirst {
            case (params, result) if params.asterism.toList.contains(target) => result
          }
        }
      ImagingTargetAndResults(target, targetResults)
    }

  private def imagingBrightestTarget: Option[ImagingTargetAndResults] =
    // TODO: Find the brightest target for imaging, I think it depends on the filter too
    // For now, just pick the first target with successful results
    imagingTargetResults.find(_.result.exists(_.isRight))

  def imagingBrightestOrFirst: Option[ImagingTargetAndResults] =
    imagingBrightestTarget
      .orElse(imagingTargetResults.headOption)

  def imagingBrightestOrFirstFromTargets(
    targets: List[ItcTarget]
  ): Option[ImagingTargetAndResults] =
    imagingBrightestOrFirst.orElse {
      // If no calculation results yet, create from observation targets
      targets.headOption.map(target => ImagingTargetAndResults(target, None))
    }

object ItcTileState:
  def Empty: ItcTileState = ItcTileState(Pot.pending, Pot.pending, none, none)

  val asterismResults = Focus[ItcTileState](_.asterismResults)

  val selectedTarget = Focus[ItcTileState](_.selectedTarget)

  val calculationResults = Focus[ItcTileState](_.calculationResults)

  val selectedImagingTarget = Focus[ItcTileState](_.selectedImagingTarget)
