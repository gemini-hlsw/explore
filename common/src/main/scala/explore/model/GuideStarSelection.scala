// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.ReactCats.*
import lucuma.ags.AgsAnalysis
import lucuma.core.math.Angle

sealed trait GuideStarSelection derives Eq

object GuideStarSelection:
  // Automatic selection, store the index
  case class AgsSelection(index: Option[(Int, AgsAnalysis)]) extends GuideStarSelection derives Eq

  // Remote selection based on the name
  case class RemoteGSSelection(name: NonEmptyString) extends GuideStarSelection derives Eq

  // Fully developed override, include analysis and location on list of candidates
  case class AgsOverride(
    selectedGSName:  NonEmptyString,
    selectedGSIndex: Int,
    analysis:        AgsAnalysis
  ) extends GuideStarSelection
      derives Eq

  val Default: GuideStarSelection = AgsSelection(None)

  extension (gs: GuideStarSelection)
    def fold[A](f: AgsSelection => A, g: RemoteGSSelection => A, h: AgsOverride => A): A =
      gs match
        case a @ AgsSelection(_)  => f(a)
        case a: RemoteGSSelection => g(a)
        case a: AgsOverride       => h(a)

    def isOverride: Boolean = fold(_ => false, _ => false, _ => true)

    def idx: Option[Int] = fold(_.index.map(_._1), _ => none, _.selectedGSIndex.some)

    def name: Option[NonEmptyString] = fold(_.index.map(_._2.target.name), _.name.some, _.name)

    def analysis: Option[AgsAnalysis] = gs match
      case AgsSelection(Some((_, a))) => a.some
      case AgsOverride(_, _, a)       => a.some
      case _                          => none

    def selectedAngle: Option[Angle] = analysis.flatMap(_.posAngle)

extension (r: List[AgsAnalysis])
  def pick(i: Int): GuideStarSelection =
    r.lift(i)
      .fold[GuideStarSelection](GuideStarSelection.AgsSelection(none))(a =>
        GuideStarSelection.AgsOverride(a.target.name, i, a)
      )

  def pick(s: NonEmptyString): GuideStarSelection =
    r.zipWithIndex
      .collectFirst { case (a, i) if a.target.name === s => i }
      .fold[GuideStarSelection](GuideStarSelection.RemoteGSSelection(s))(i =>
        GuideStarSelection.AgsOverride(s, i, r(i))
      )
