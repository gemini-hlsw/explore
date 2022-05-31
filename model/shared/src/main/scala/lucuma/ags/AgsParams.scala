// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.Eq
import cats.syntax.all._
import lucuma.core.enum.GmosNorthFpu
import lucuma.core.enum.GmosSouthFpu
import lucuma.core.enum.PortDisposition
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.gmos.probeArm
import lucuma.core.geom.gmos.scienceArea
import lucuma.core.geom.jts.interpreter._
import lucuma.core.geom.syntax.all._
import lucuma.core.math.Angle
import lucuma.core.math.Offset

final case class AgsPosition(posAngle: Angle, offsetPos: Offset)

object AgsPosition {
  implicit val agsPositionEq: Eq[AgsPosition] = Eq.by(x => (x.posAngle, x.offsetPos))
}

sealed trait AgsParams {
  def probe: GuideProbe

  def isReachable(gsOffset: Offset, position: AgsPosition): Boolean

  def vignettingArea(position: AgsPosition)(gsOffset: Offset): ShapeExpression
}

object AgsParams {

  final case class GmosAgsParams(
    fpu:  Option[Either[GmosNorthFpu, GmosSouthFpu]],
    port: PortDisposition
  ) extends AgsParams {
    val probe = GuideProbe.OIWFS

    def isReachable(gsOffset: Offset, position: AgsPosition): Boolean =
      patrolField(position).eval.contains(gsOffset)

    def patrolField(position: AgsPosition): ShapeExpression =
      probeArm.patrolFieldAt(position.posAngle, position.offsetPos, fpu, port)

    override def vignettingArea(position: AgsPosition)(gsOffset: Offset): ShapeExpression =
      scienceArea.shapeAt(position.posAngle, position.offsetPos, fpu) ∩
        probeArm.shapeAt(position.posAngle, gsOffset, position.offsetPos, fpu, port)

    implicit val gmosParamsEq: Eq[GmosAgsParams] = Eq.by(x => (x.fpu, x.port))
  }

  implicit val agsParamsEq: Eq[AgsParams] = Eq.instance {
    case (GmosAgsParams(f1, p1), GmosAgsParams(f2, p2)) => f1 === f2 && p1 === p2
    case _                                              => false
  }
}
