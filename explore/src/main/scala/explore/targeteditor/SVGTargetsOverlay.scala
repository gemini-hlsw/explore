// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Eq
import cats.syntax.all._
import crystal.react.reuse._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.svg_<^._
import lucuma.core.math.Coordinates
import lucuma.ui.reusability._
import react.common._
import react.common.implicits._

sealed trait SVGTarget {
  def coordinates: Coordinates
  def css: Css
}

object SVGTarget {
  final case class CircleTarget(
    coordinates: Coordinates,
    css:         Css,
    radius:      Double,
    title:       Option[String] = None
  ) extends SVGTarget

  final case class CrosshairTarget(
    coordinates: Coordinates,
    css:         Css,
    side:        Double,
    title:       Option[String] = None
  ) extends SVGTarget

  final case class LineTo(
    coordinates: Coordinates,
    destination: Coordinates,
    css:         Css,
    title:       Option[String] = None
  ) extends SVGTarget

  final case class GuideStarCandidateTarget(
    coordinates: Coordinates,
    css:         Css,
    radius:      Double,
    title:       Option[String] = None
  ) extends SVGTarget

  implicit val eqSVGTarget: Eq[SVGTarget] = Eq.instance {
    case (CircleTarget(c1, s1, r1, t1), CircleTarget(c2, s2, r2, t2))       =>
      c1 === c2 && s1 === s2 & r1 === r2 && t1 === t2
    case (CrosshairTarget(c1, s1, r1, t1), CrosshairTarget(c2, s2, r2, t2)) =>
      c1 === c2 && s1 === s2 & r1 === r2 && t1 === t2
    case _                                                                  => false
  }

  implicit val svgTargetReusability: Reusability[SVGTarget] = Reusability.byEq
}

final case class SVGTargetsOverlay(
  width:     Int,
  height:    Int,
  world2pix: Coordinates ==> Option[(Double, Double)],
  targets:   List[SVGTarget]
) extends ReactFnPropsWithChildren[SVGTargetsOverlay](SVGTargetsOverlay.component)

object SVGTargetsOverlay {
  type Props = SVGTargetsOverlay
  implicit val reuse: Reusability[Props] = Reusability.derive

  val canvasWidth  = VdomAttr("width")
  val canvasHeight = VdomAttr("height")

  val component =
    ScalaFnComponent
      .withChildren[Props] { (p, c) =>
        val svg = <.svg(
          Css("targets-overlay-svg"),
          canvasWidth  := s"${p.width}px",
          canvasHeight := s"${p.height}px",
          p.targets
            .map(c => (c, p.world2pix(c.coordinates)))
            .collect {
              case (SVGTarget.CircleTarget(_, css, radius, title), Some((x, y)))             =>
                val pointCss = Css("circle-target") |+| css
                <.circle(^.cx := x, ^.cy := y, ^.r := radius, pointCss, title.map(<.title(_)))
              case (SVGTarget.GuideStarCandidateTarget(_, css, radius, title), Some((x, y))) =>
                val pointCss = Css("guide-star-candidate-target") |+| css
                <.circle(^.cx := x, ^.cy := y, ^.r := radius, pointCss, title.map(<.title(_)))
              case (SVGTarget.CrosshairTarget(_, css, side, title), Some((x, y)))            =>
                val pointCss = Css("crosshair-target") |+| css
                <.g(
                  <.line(^.x1 := x - side, ^.x2 := x + side, ^.y1 := y, ^.y2        := y, pointCss),
                  <.line(^.x1 := x, ^.x2        := x, ^.y1        := y - side, ^.y2 := y + side, pointCss),
                  title.map(<.title(_))
                )
              case (SVGTarget.LineTo(_, d, css, title), Some((x, y)))                        =>
                val pointCss = Css("arrow-between-target") |+| css
                p.world2pix(d)
                  .map { case (x1, y1) =>
                    <.line(^.x1 := x,
                           ^.x2 := x1,
                           ^.y1 := y,
                           ^.y2 := y1,
                           pointCss,
                           title.map(<.title(_))
                    )
                  }
                  .getOrElse(EmptyVdom)
            }
            .toTagMod
        )
        svg(c)
      }
}
