// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Eq
import cats.syntax.all._
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.svg_<^._
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.ui.reusability._
import react.aladin.Fov
import react.common._
import react.common.implicits._

import scala.math._

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

  final case class GuideStarTarget(
    coordinates: Coordinates,
    css:         Css,
    radius:      Double,
    title:       Option[String] = None
  ) extends SVGTarget

  implicit val eqSVGTarget: Eq[SVGTarget] = Eq.instance {
    case (CircleTarget(c1, s1, r1, t1), CircleTarget(c2, s2, r2, t2))                         =>
      c1 === c2 && s1 === s2 & r1 === r2 && t1 === t2
    case (CrosshairTarget(c1, s1, r1, t1), CrosshairTarget(c2, s2, r2, t2))                   =>
      c1 === c2 && s1 === s2 & r1 === r2 && t1 === t2
    case (GuideStarCandidateTarget(c1, s1, r1, t1), GuideStarCandidateTarget(c2, s2, r2, t2)) =>
      c1 === c2 && s1 === s2 & r1 === r2 && t1 === t2
    case (GuideStarTarget(c1, s1, r1, t1), GuideStarTarget(c2, s2, r2, t2))                   =>
      c1 === c2 && s1 === s2 & r1 === r2 && t1 === t2
    case (LineTo(c1, d1, r1, t1), LineTo(c2, d2, r2, t2))                                     =>
      c1 === c2 && d1 === d2 & r1 === r2 && t1 === t2
    case _                                                                                    => false
  }

  implicit val svgTargetReusability: Reusability[SVGTarget] = Reusability.byEq
}

final case class TargetsOverlay(
  width:           Int,
  height:          Int,
  fov:             Fov,
  screenOffset:    Offset,
  baseCoordinates: Coordinates,
  targets:         List[SVGTarget]
) extends ReactFnProps[TargetsOverlay](TargetsOverlay.component)

object TargetsOverlay {
  type Props = TargetsOverlay
  implicit val doubleReuse: Reusability[Double] = Reusability.double(1)
  implicit val exactFovReuse: Reusability[Fov]  = Reusability.derive
  implicit val reuse: Reusability[Props]        = Reusability.derive

  val JtsSvg    = Css("targets-overlay-svg")
  val JtsGuides = Css("viz-guides")

  val scale = (v: Double) => rint(v / 1000)

  val canvasWidth  = VdomAttr("width")
  val canvasHeight = VdomAttr("height")
  val component    =
    ScalaFnComponent
      .withReuse[Props] { p =>
        val (x, y, maxX, maxY) =
          p.targets.foldLeft((Double.MaxValue, Double.MaxValue, Double.MinValue, Double.MinValue)) {
            case ((x, y, w, h), target) =>
              val offset = target.coordinates.diff(p.baseCoordinates).offset
              // Offset amount
              val offP   =
                Offset.P.signedDecimalArcseconds.get(offset.p).toDouble * 1e6

              val offQ =
                Offset.Q.signedDecimalArcseconds.get(offset.q).toDouble * 1e6
              (x.min(offP), y.min(offQ), w.max(offP), h.max(offQ))
          }

        val w = abs(maxX - x)
        val h = abs(maxY - y)

        // Shift factors on x/y, basically the percentage shifted on x/y
        val px   = abs(x / w) - 0.5
        val py   = abs(y / h) - 0.5
        // scaling factors on x/y
        val sx   = p.fov.x.toMicroarcseconds / w
        val sy   = p.fov.y.toMicroarcseconds / h
        val pixx = p.fov.x.toMicroarcseconds / p.width
        val pixy = p.fov.y.toMicroarcseconds / p.height
        val maxP = max(pixx, pixy)

        // Offset amount
        val offP =
          Offset.P.signedDecimalArcseconds.get(p.screenOffset.p).toDouble * 1e6

        val offQ =
          Offset.Q.signedDecimalArcseconds.get(p.screenOffset.q).toDouble * 1e6

        val viewBoxX = scale(x + px * w) * sx + scale(offP)
        val viewBoxY = scale(y + py * h) * sy + scale(offQ)
        val viewBoxW = scale(w) * sx
        val viewBoxH = scale(h) * sy

        val viewBox = s"$viewBoxX $viewBoxY $viewBoxW $viewBoxH"
        val svg     = <.svg(
          JtsSvg,
          ^.viewBox    := viewBox,
          canvasWidth  := s"${p.width}px",
          canvasHeight := s"${p.height}px",
          p.targets
            .fmap { t =>
              val offset = t.coordinates.diff(p.baseCoordinates).offset
              // Offset amount
              val offP   =
                Offset.P.signedDecimalArcseconds.get(offset.p).toDouble * 1e6

              val offQ =
                Offset.Q.signedDecimalArcseconds.get(offset.q).toDouble * 1e6
              (offP, offQ, t)
            }
            .collect {
              case (offP, offQ, SVGTarget.CircleTarget(_, css, radius, title))             =>
                val pointCss = ExploreStyles.CircleTarget |+| css

                <.circle(^.cx := scale(offP),
                         ^.cy := scale(offQ),
                         ^.r  := scale(maxP * radius),
                         pointCss,
                         title.map(<.title(_))
                )
              case (offP, offQ, SVGTarget.CrosshairTarget(_, css, sidePx, title))          =>
                val pointCss = ExploreStyles.CrosshairTarget |+| css

                val side = scale(maxP * sidePx)
                <.g(
                  <.line(^.x1 := scale(offP) - side,
                         ^.x2 := scale(offP) + side,
                         ^.y1 := scale(offQ),
                         ^.y2 := scale(offQ),
                         pointCss
                  ),
                  <.line(^.x1 := scale(offP),
                         ^.x2 := scale(offP),
                         ^.y1 := scale(offQ) - side,
                         ^.y2 := scale(offQ) + side,
                         pointCss
                  ),
                  title.map(<.title(_))
                )
              case (offP, offQ, SVGTarget.GuideStarCandidateTarget(_, css, radius, title)) =>
                val pointCss = ExploreStyles.GuideStarCandidateTarget |+| css
                <.circle(^.cx := scale(offP),
                         ^.cy := scale(offQ),
                         ^.r  := scale(maxP * radius),
                         pointCss,
                         title.map(<.title(_))
                )
              case (offP, offQ, SVGTarget.GuideStarTarget(_, css, radius, title))          =>
                val pointCss = ExploreStyles.GuideStarTarget |+| css
                <.circle(^.cx := scale(offP),
                         ^.cy := scale(offQ),
                         ^.r  := scale(maxP * radius),
                         pointCss,
                         title.map(<.title(_))
                )
              case (offP, offQ, SVGTarget.LineTo(_, d, css, title))                        =>
                val destOffset = d.diff(p.baseCoordinates).offset
                // Offset amount
                val destP      =
                  Offset.P.signedDecimalArcseconds.get(destOffset.p).toDouble * 1e6

                val destQ =
                  Offset.Q.signedDecimalArcseconds.get(destOffset.q).toDouble * 1e6

                val pointCss = ExploreStyles.ArrowBetweenTargets |+| css
                <.line(^.x1 := scale(offP),
                       ^.x2 := scale(destP),
                       ^.y1 := scale(offQ),
                       ^.y2 := scale(destQ),
                       pointCss,
                       title.map(<.title(_))
                )
              // case (SVGTarget.GuideStarTarget(_, css, radius, title), Some((x, y)))          =>
            }
            .toTagMod
        )
        svg
      }
}
