// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Semigroup
import cats.data.NonEmptyMap
import cats.syntax.all._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.svg_<^._
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.jts.JtsShape
import lucuma.core.geom.jts.interpreter._
import lucuma.core.math.Offset
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryCollection
import org.locationtech.jts.geom.Polygon
import react.aladin.Fov
import react.common._
import react.common.implicits._

import scala.math._

final case class SVGVisualizationOverlay(
  width:        Int,
  height:       Int,
  fov:          Fov,
  screenOffset: Offset,
  shapes:       NonEmptyMap[Css, ShapeExpression],
  clazz:        Css = Css.Empty
) extends ReactFnProps[SVGVisualizationOverlay](SVGVisualizationOverlay.component)

object SVGVisualizationOverlay {
  type Props = SVGVisualizationOverlay

  val geometryUnionSemigroup: Semigroup[Geometry] =
    Semigroup.instance(_.union(_))

  // The values on the geometry are in microarcseconds
  // They are fairly large and break is some browsers
  // We apply a scaling factor uniformily
  val scale = (v: Double) => rint(v / 1000)

  val JtsPolygon    = Css("viz-polygon")
  val JtsCollection = Css("viz-collecttion")
  val JtsGuides     = Css("viz-guides")
  val JtsSvg        = Css("visualization-overlay-svg")

  def forGeometry(css: Css, g: Geometry): VdomNode =
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

  val canvasWidth  = VdomAttr("width")
  val canvasHeight = VdomAttr("height")

  val component =
    ScalaFnComponent[Props] { p =>
      // Render the svg
      val evald: NonEmptyMap[Css, JtsShape] = p.shapes
        .fmap(_.eval)
        .map {
          case jts: JtsShape => jts
          case x             => sys.error(s"Whoa unexpected shape type: $x")
        }
      val composite                         = evald
        .map(_.g)
        .reduce(geometryUnionSemigroup)

      val envelope = composite.getBoundary.getEnvelopeInternal

      // We should calculate the viewbox of the whole geometry
      // dimension in micro arcseconds
      val (x, y, w, h) =
        (envelope.getMinX, envelope.getMinY, envelope.getWidth, envelope.getHeight)

      // Shift factors on x/y, basically the percentage shifted on x/y
      val px = abs(x / w) - 0.5
      val py = abs(y / h) - 0.5
      // scaling factors on x/y
      val sx = p.fov.x.toMicroarcseconds / w
      val sy = p.fov.y.toMicroarcseconds / h

      // Offset amount
      val offP =
        Offset.P.signedDecimalArcseconds.get(p.screenOffset.p).toDouble * 1e6

      val offQ =
        Offset.Q.signedDecimalArcseconds.get(p.screenOffset.q).toDouble * 1e6

      // Do the shifting and offseting via viewbox
      val viewBoxX = scale(x + px * w) * sx + scale(offP)
      val viewBoxY = scale(y + py * h) * sy + scale(offQ)
      val viewBoxW = scale(w) * sx
      val viewBoxH = scale(h) * sy

      val viewBox = s"$viewBoxX $viewBoxY $viewBoxW $viewBoxH"

      val svg = <.svg(
        JtsSvg |+| p.clazz,
        ^.viewBox    := viewBox,
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
