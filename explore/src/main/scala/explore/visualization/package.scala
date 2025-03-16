// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.visualization

import cats.Semigroup
import lucuma.ags.GuideStarCandidate
import lucuma.core.math.Offset
import lucuma.core.util.NewBoolean
import lucuma.ui.aladin.Fov
import lucuma.react.common.Css
import org.locationtech.jts.geom.Geometry

import scala.math.*

// The values on the geometry are in microarcseconds
// They are fairly large and break is some browsers
// We apply a scaling factor uniformil
inline def scale: Double => Double = (v: Double) => rint(v / 1000)

inline def reverseScale: Double => Double = (v: Double) => rint(v * 1000)

val geometryUnionSemigroup: Semigroup[Geometry] =
  Semigroup.instance(_.union(_))

extension (offset: Offset)
  def micros: (Double, Double) = {
    // Offset amount
    val offP =
      Offset.P.signedDecimalArcseconds.get(offset.p).toDouble * 1e6

    val offQ =
      Offset.Q.signedDecimalArcseconds.get(offset.q).toDouble * 1e6
    (offP, offQ)
  }

def calculateViewBox(
  x:            Double,
  y:            Double,
  w:            Double,
  h:            Double,
  fov:          Fov,
  screenOffset: Offset
): (Double, Double, Double, Double) = {
  // Shift factors on x/y, basically the percentage shifted on x/y
  val px           = abs(x / w) - 0.5
  val py           = abs(y / h) - 0.5
  // scaling factors on x/y
  val sx           = fov.x.toMicroarcseconds / w
  val sy           = fov.y.toMicroarcseconds / h
  // Offset amount
  val (offP, offQ) = screenOffset.micros

  val viewBoxX = scale(x + px * w) * sx + scale(offP)
  val viewBoxY = scale(y + py * h) * sy + scale(offQ)
  val viewBoxW = scale(w) * sx
  val viewBoxH = scale(h) * sy
  (viewBoxX, viewBoxY, viewBoxW, viewBoxH)
}

object TooltipState extends NewBoolean { inline def Open = True; inline def Closed = False }
type TooltipState = TooltipState.Type

/**
 * Path for a tooltip located above the point
 * https://medium.com/welldone-software/tooltips-using-svg-path-1bd69cc7becd
 */
def topTooltipPath(width: Double, height: Double, offset: Double, radius: Double): String =
  val left   = -width / 2
  val right  = width / 2
  val top    = -offset - height
  val bottom = -offset

  s"""M 0,0
    L ${-offset},${bottom}
    H ${left + radius}
    Q ${left},${bottom} ${left},${bottom - radius}
    V ${top + radius}
    Q ${left},${top} ${left + radius},${top}
    H ${right - radius}
    Q ${right},${top} ${right},${top + radius}
    V ${bottom - radius}
    Q ${right},${bottom} ${right - radius},${bottom}
    H ${offset}
    L 0,0 z""".stripMargin

/**
 * Path for a tooltip located below the point
 */
def bottomTooltipPath(width: Double, height: Double, offset: Double, radius: Double): String =
  val left   = -width / 2
  val right  = width / 2
  val bottom = offset + height
  val top    = offset
  s"""M 0,0
    L ${-offset},${top}
    H ${left + radius}
    Q ${left},${top} ${left},${top + radius}
    V ${bottom - radius}
    Q ${left},${bottom} ${left + radius},${bottom}
    H ${right - radius}
    Q ${right},${bottom} ${right},${bottom - radius}
    V ${top + radius}
    Q ${right},${top} ${right - radius},${top}
    H ${offset}
    L 0,0 z""".stripMargin

def textDomSize(textValue: String): (Double, Double) =
  val document = org.scalajs.dom.document
  val text     = document.createElement("span")
  document.body.appendChild(text)

  text.innerHTML = textValue;

  val width  = Math.ceil(text.clientWidth)
  val height = Math.ceil(text.clientHeight)

  document.body.removeChild(text)
  (width, height)

extension (target: GuideStarCandidate)
  protected def selector: Css =
    Css(s"guide-star-${target.id}")
