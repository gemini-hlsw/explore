// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.Semigroup
import lucuma.core.math.Offset
import org.locationtech.jts.geom.Geometry
import react.aladin.Fov

import scala.math._

package object visualization {
  // The values on the geometry are in microarcseconds
  // They are fairly large and break is some browsers
  // We apply a scaling factor uniformil
  val scale: Double => Double = (v: Double) => rint(v / 1000)

  val geometryUnionSemigroup: Semigroup[Geometry] =
    Semigroup.instance(_.union(_))

  implicit class OffsetOps(val offset: Offset) extends AnyVal {
    def micros: (Double, Double) = {
      // Offset amount
      val offP =
        Offset.P.signedDecimalArcseconds.get(offset.p).toDouble * 1e6

      val offQ =
        Offset.Q.signedDecimalArcseconds.get(offset.q).toDouble * 1e6
      (offP, offQ)
    }
  }

  def calculateViewBox(
    x:            Double,
    y:            Double,
    w:            Double,
    h:            Double,
    fov:          Fov,
    screenOffset: Offset
  ): String = {
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
    s"$viewBoxX $viewBoxY $viewBoxW $viewBoxH"
  }
}
