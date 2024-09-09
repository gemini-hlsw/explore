// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.option.*
import cats.syntax.eq.*
import explore.model.AsterismVisualOptions
import japgolly.scalajs.react.*
import lucuma.core.util.NewType
import lucuma.react.aladin.Fov
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.document
import lucuma.ags.AgsAnalysis
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.cats.*

extension (options: AsterismVisualOptions) inline def fov: Fov = Fov(options.fovRA, options.fovDec)

extension (fov: Fov)
  def isDifferentEnough(newFov: Fov): Boolean =
    (fov.x.toMicroarcseconds - newFov.x.toMicroarcseconds).abs < 1e7 ||
      (fov.y.toMicroarcseconds - newFov.y.toMicroarcseconds).abs < 1e7

object AddDisabled extends NewType[Boolean]
type AddDisabled = AddDisabled.Type

val domRoot: Option[HTMLElement] =
  Option(document.querySelector(":root")) match
    case Some(r: HTMLElement) => r.some
    case _                    => none

def setVariable(root: Option[HTMLElement], variableName: String, value: Int): Callback =
  root
    .map: root =>
      Callback(root.style.setProperty(s"--aladin-image-$variableName", s"${value / 100.0}"))
    .getOrEmpty

extension (r: List[AgsAnalysis])
  def pick(i: Int): GuideStarSelection =
    r.lift(i)
      .fold[GuideStarSelection](GuideStarSelection.AgsSelection(none))(a =>
        GuideStarSelection.AgsOverride(a.target.name, i, a)
      )

  def pick(s: NonEmptyString): GuideStarSelection =
    r.zipWithIndex
      .collectFirst { case (a, i) if a.target.name === s => i }
      .fold[GuideStarSelection](GuideStarSelection.AgsSelection(none))(i =>
        GuideStarSelection.AgsOverride(s, i, r(i))
      )

sealed trait GuideStarSelection

object GuideStarSelection:
  // Automatic selection, store the index
  case class AgsSelection(index: Option[Int]) extends GuideStarSelection

  // Remote selection based on the name
  case class RemoteGSSelection(name: NonEmptyString) extends GuideStarSelection

  // Fully developed override, include analysis and location on list of candidates
  case class AgsOverride(
    selectedGSName:  NonEmptyString,
    selectedGSIndex: Int,
    analysis:        AgsAnalysis
  ) extends GuideStarSelection

  extension (gs: GuideStarSelection)
    def fold[A](f: AgsSelection => A, g: RemoteGSSelection => A, h: AgsOverride => A): A =
      gs match
        case a @ AgsSelection(_)  => f(a)
        case a: RemoteGSSelection => g(a)
        case a: AgsOverride       => h(a)

    def targetName = gs match
      case AgsSelection(_)         => none
      case RemoteGSSelection(name) => name.some
      case AgsOverride(name, _, _) => name.some

    def isOverride: Boolean = fold(_ => false, _ => false, _ => true)

    def idx: Option[Int] = fold(_.index, _ => none, _.selectedGSIndex.some)

    def guideStar(results: List[AgsAnalysis]) = gs match
      case AgsSelection(Some(i)) => results.lift(i)
      case AgsOverride(_, _, a)  => a.some
      case _                     => none
