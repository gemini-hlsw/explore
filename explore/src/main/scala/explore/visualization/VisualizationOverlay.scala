// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.visualization

import cats.data.NonEmptyMap
import cats.syntax.all.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.svg_<^.*
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.jts.JtsShape
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.math.Offset
import lucuma.ui.aladin.Fov
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.ui.syntax.all.given
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryCollection
import org.locationtech.jts.geom.Polygon

case class SVGVisualizationOverlay(
  width:        Int,
  height:       Int,
  fov:          Fov,
  screenOffset: Offset,
  shapes:       NonEmptyMap[Css, ShapeExpression],
  clazz:        Css = Css.Empty
) extends ReactFnProps(SVGVisualizationOverlay.component)

object SVGVisualizationOverlay {
  private type Props = SVGVisualizationOverlay

  val JtsPolygon    = Css("viz-polygon")
  val JtsCollection = Css("viz-collecttion")
  val JtsGuides     = Css("viz-guides")
  val JtsSvg        = Css("visualization-overlay-svg")

  private def forGeometry(css: Css, g: Geometry): VdomNode =
    g match {
      case p: Polygon            =>
        val points = p.getCoordinates
          .map(c => s"${scale(c.x)},${scale(c.y)}")
          .mkString(" ")
        <.polygon(css |+| JtsPolygon, ^.points := points)
      case p: GeometryCollection =>
        <.g(
          css |+| JtsCollection,
          p.geometries.map(forGeometry(css, _)).toTagMod
        )
      case _                     => EmptyVdom
    }

  private val component =
    ScalaFnComponent[Props] { p =>
      // Render the svg
      val evald: NonEmptyMap[Css, JtsShape] = p.shapes
        .fmap(_.eval)
        .map {
          case jts: JtsShape => jts
          case x             => sys.error(s"Whoa unexpected shape type: $x")
        }

      val composite = evald
        .map(_.g)
        .reduce(geometryUnionSemigroup)

      val envelope = composite.getBoundary.getEnvelopeInternal

      // We should calculate the viewbox of the whole geometry
      // dimension in micro arcseconds
      val (x, y, w, h) =
        (envelope.getMinX, envelope.getMinY, envelope.getWidth, envelope.getHeight)

      val (viewBoxX, viewBoxY, viewBoxW, viewBoxH) =
        calculateViewBox(x, y, w, h, p.fov, p.screenOffset)

      val svg = <.svg(
        JtsSvg |+| p.clazz,
        ^.viewBox    := s"$viewBoxX $viewBoxY $viewBoxW $viewBoxH",
        canvasWidth  := s"${p.width}px",
        canvasHeight := s"${p.height}px",
        <.g(
          ^.transform := s"scale(1, -1)",
          evald.toNel
            .map { case (css, shape) =>
              forGeometry(css, shape.g)
            }
            .toList
            .toTagMod
        )
      )
      svg
    }
}
