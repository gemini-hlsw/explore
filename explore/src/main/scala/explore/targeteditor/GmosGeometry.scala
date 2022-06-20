// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Order
import cats.data.NonEmptyMap
import cats.syntax.all._
import explore.model.ScienceMode
import explore.model.ScienceModeBasic
import lucuma.core.enums.PortDisposition
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.gmos
import lucuma.core.geom.syntax.shapeexpression._
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import react.common.implicits._
import react.common.style.Css

/**
 * Test object to produce a gmos geometry. it is for demo purposes only
 */
object GmosGeometry {

  // Move to react common
  implicit val cssOrder: Order[Css] = Order.by(_.htmlClass)

  // Shape to display for a specific mode
  def shapesForMode(
    posAngle: Angle,
    mode:     Option[ScienceMode],
    port:     PortDisposition
  ): NonEmptyMap[Css, ShapeExpression] =
    mode match {
      case Some(ScienceMode.GmosNorthLongSlit(ScienceModeBasic.GmosNorthLongSlit(_, _, fpu), _)) =>
        NonEmptyMap.of(
          (Css("gmos-science-ccd"), gmos.scienceArea.imaging ⟲ posAngle),
          (Css("gmos-fpu"), gmos.scienceArea.shapeAt(posAngle, Offset.Zero, fpu.asLeft.some)),
          (Css("gmos-patrol-field"),
           gmos.probeArm.patrolFieldAt(posAngle, Offset.Zero, fpu.asLeft.some, port)
          )
        )
      case Some(ScienceMode.GmosSouthLongSlit(ScienceModeBasic.GmosSouthLongSlit(_, _, fpu), _)) =>
        NonEmptyMap.of(
          (Css("gmos-science-ccd"), gmos.scienceArea.imaging ⟲ posAngle),
          (Css("gmos-fpu"), gmos.scienceArea.shapeAt(posAngle, Offset.Zero, fpu.asRight.some)),
          (Css("gmos-patrol-field"),
           gmos.probeArm.patrolFieldAt(posAngle, Offset.Zero, fpu.asRight.some, port)
          )
        )
      case _                                                                                     =>
        NonEmptyMap.of(
          (Css("gmos-science-ccd"), gmos.scienceArea.imaging ⟲ posAngle)
        )
    }

  // Shape to display always
  def commonShapes(posAngle: Angle, extraCss: Css): NonEmptyMap[Css, ShapeExpression] =
    NonEmptyMap.of(
      (Css("gmos-candidates-area") |+| extraCss,
       gmos.probeArm.candidatesAreaAt(posAngle, Offset.Zero)
      )
    )

  // Shape to display always
  def probeShapes(
    posAngle:        Angle,
    guideStarOffset: Offset,
    offsetPos:       Offset,
    mode:            Option[ScienceMode],
    port:            PortDisposition,
    extraCss:        Css
  ): NonEmptyMap[Css, ShapeExpression] =
    mode match {
      case Some(ScienceMode.GmosNorthLongSlit(ScienceModeBasic.GmosNorthLongSlit(_, _, fpu), _)) =>
        NonEmptyMap.of(
          (Css("gmos-probe-arm") |+| extraCss,
           gmos.probeArm.shapeAt(posAngle, guideStarOffset, offsetPos, fpu.asLeft.some, port)
          )
        )
      case Some(ScienceMode.GmosSouthLongSlit(ScienceModeBasic.GmosSouthLongSlit(_, _, fpu), _)) =>
        NonEmptyMap.of(
          (Css("gmos-probe-arm") |+| extraCss,
           gmos.probeArm.shapeAt(posAngle, guideStarOffset, offsetPos, fpu.asRight.some, port)
          )
        )
      case _                                                                                     =>
        NonEmptyMap.of(
          (Css("gmos-science-ccd"), gmos.scienceArea.imaging ⟲ posAngle)
        )
    }

}
