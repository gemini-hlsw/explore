// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.visualization

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import explore.components.ui.ExploreStyles
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.svg_<^.*
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.given
import org.scalajs.dom.svg.SVG
import react.aladin.Fov
import react.common.Css
import react.common.ReactFnProps

import scala.math.*

sealed trait SVGTarget derives Eq {
  def coordinates: Coordinates
  def css: Css
}

object SVGTarget {
  case class CircleTarget(
    coordinates: Coordinates,
    css:         Css,
    radius:      Double,
    title:       Option[String] = None
  ) extends SVGTarget
      derives Eq

  case class CrosshairTarget(
    coordinates: Coordinates,
    css:         Css,
    side:        Double,
    title:       Option[String] = None
  ) extends SVGTarget
      derives Eq

  case class ScienceTarget(
    coordinates: Coordinates,
    css:         Css,
    selectedCss: Css,
    side:        Double,
    selected:    Boolean,
    title:       Option[String] = None
  ) extends SVGTarget
      derives Eq

  case class LineTo(
    coordinates: Coordinates,
    destination: Coordinates,
    css:         Css,
    title:       Option[String] = None
  ) extends SVGTarget
      derives Eq

  case class GuideStarCandidateTarget(
    coordinates: Coordinates,
    css:         Css,
    radius:      Double,
    title:       Option[String] = None
  ) extends SVGTarget
      derives Eq

  case class GuideStarTarget(
    coordinates: Coordinates,
    css:         Css,
    radius:      Double,
    title:       Option[String] = None
  ) extends SVGTarget
      derives Eq

  given Reusability[SVGTarget] = Reusability.byEq
}

case class TargetsOverlay(
  width:           Int,
  height:          Int,
  fov:             Fov,
  screenOffset:    Offset,
  baseCoordinates: Coordinates,
  targets:         List[SVGTarget]
) extends ReactFnProps(TargetsOverlay.component)

object TargetsOverlay {
  type Props = TargetsOverlay
  given Reusability[Double] = Reusability.double(1)
  given Reusability[Fov]    = Reusability.derive
  given Reusability[Props]  = Reusability.derive

  val JtsSvg     = Css("targets-overlay-svg")
  val JtsTargets = Css("overlay-all-targets")
  val JtsGuides  = Css("viz-guides")

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useRefToVdom[SVG] // svg
      .render { (p, svgRef) =>
        val pixx = p.fov.x.toMicroarcseconds / p.width
        val pixy = p.fov.y.toMicroarcseconds / p.height
        val maxP = max(pixx, pixy)

        val (x0, y0, maxX, maxY, minSide) =
          p.targets.foldLeft(
            (Double.MaxValue, Double.MaxValue, Double.MinValue, Double.MinValue, 0.0)
          ) { case ((x, y, w, h, s), target) =>
            val side         = target match {
              case SVGTarget.CrosshairTarget(_, _, sidePx, _) =>
                maxP * sidePx
              case _                                          =>
                0.0
            }
            val offset       = target.coordinates.diff(p.baseCoordinates).offset
            // Offset amount
            val (offP, offQ) = offset.micros
            (x.min(offP), y.min(offQ), w.max(offP), h.max(offQ), s.max(side))
          }

        val w0 = abs(maxX - x0)
        val h0 = abs(maxY - y0)

        val (x, y, w, h) =
          if (w0 == 0 || h0 == 0) (x0 - 2 * minSide, y0 - 2 * minSide, minSide * 2, minSide * 2)
          else (x0, y0, w0, h0)

        val (viewBoxX, viewBoxY, viewBoxW, viewBoxH) =
          calculateViewBox(x, y, w, h, p.fov, p.screenOffset)

        val svg = <.svg(
          JtsSvg,
          ^.untypedRef := svgRef,
          ^.viewBox    := s"$viewBoxX $viewBoxY $viewBoxW $viewBoxH",
          canvasWidth  := s"${p.width}px",
          canvasHeight := s"${p.height}px",
          <.g(
            JtsTargets,
            p.targets
              .fmap { t =>
                val offset       = t.coordinates.diff(p.baseCoordinates).offset
                // Offset amount
                val (offP, offQ) = offset.micros
                (offP, offQ, t)
              }
              .collect {
                case (offP, offQ, SVGTarget.CircleTarget(_, css, radius, title))    =>
                  val pointCss = ExploreStyles.CircleTarget |+| css

                  <.circle(^.cx := scale(offP),
                           ^.cy := scale(offQ),
                           ^.r  := scale(maxP * radius),
                           pointCss,
                           title.map(<.title(_))
                  )
                case (offP, offQ, SVGTarget.CrosshairTarget(_, css, sidePx, title)) =>
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
                case (offP,
                      offQ,
                      SVGTarget.ScienceTarget(_, css, selectedCss, sidePx, title, selected)
                    ) =>
                  val pointCss = ExploreStyles.CrosshairTarget |+| css
                  val sx       = p.width / (viewBoxW - viewBoxX).abs
                  val sy       = p.height / (viewBoxH - viewBoxY).abs

                  CrossTarget(Option(svgRef.raw.current).map(_.asInstanceOf[SVG]),
                              offP,
                              offQ,
                              maxP,
                              sidePx,
                              pointCss,
                              selectedCss,
                              sx,
                              sy,
                              title,
                              selected
                  ): VdomNode

                case (offP, offQ, SVGTarget.GuideStarCandidateTarget(_, css, radius, title)) =>
                  val pointCss = ExploreStyles.GuideStarCandidateTarget |+| css
                  <.circle(^.cx := scale(offP),
                           ^.cy := scale(offQ),
                           ^.r  := scale(maxP * radius),
                           pointCss,
                           title.map(<.title(_))
                  )

                case (offP, offQ, SVGTarget.GuideStarTarget(_, css, radius, title)) =>
                  val pointCss = ExploreStyles.GuideStarTarget |+| css
                  <.circle(^.cx := scale(offP),
                           ^.cy := scale(offQ),
                           ^.r  := scale(maxP * radius),
                           pointCss,
                           title.map(<.title(_))
                  )
                case (offP, offQ, SVGTarget.LineTo(_, d, css, title))               =>
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
              }
              .toTagMod
          )
        )
        svg
      }
}
