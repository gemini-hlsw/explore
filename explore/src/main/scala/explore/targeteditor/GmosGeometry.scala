// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Order
import cats.data.NonEmptyMap
import cats.syntax.all.*
import explore.model.ScienceMode
import lucuma.core.enums.PortDisposition
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.gmos
import lucuma.core.geom.syntax.shapeexpression.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import react.common.implicits.*
import react.common.style.Css

/**
 * Test object to produce a gmos geometry. it is for demo purposes only
 */
object GmosGeometry:

  // Shape to display for a specific mode
  def shapesForMode(
    posAngle: Angle,
    mode:     Option[ScienceMode],
    port:     PortDisposition
  ): NonEmptyMap[Css, ShapeExpression] =
    mode match {
      case Some(m: ScienceMode.GmosNorthLongSlit) =>
        NonEmptyMap.of(
          (Css("gmos-science-ccd"), gmos.scienceArea.imaging ⟲ posAngle),
          (Css("gmos-fpu"), gmos.scienceArea.shapeAt(posAngle, Offset.Zero, m.fpu.asLeft.some)),
          (Css("gmos-patrol-field"),
           gmos.probeArm.patrolFieldAt(posAngle, Offset.Zero, m.fpu.asLeft.some, port)
          )
        )
      case Some(m: ScienceMode.GmosSouthLongSlit) =>
        NonEmptyMap.of(
          (Css("gmos-science-ccd"), gmos.scienceArea.imaging ⟲ posAngle),
          (Css("gmos-fpu"), gmos.scienceArea.shapeAt(posAngle, Offset.Zero, m.fpu.asRight.some)),
          (Css("gmos-patrol-field"),
           gmos.probeArm.patrolFieldAt(posAngle, Offset.Zero, m.fpu.asRight.some, port)
          )
        )
      case _                                      =>
        NonEmptyMap.of(
          (Css("gmos-science-ccd"), gmos.scienceArea.imaging ⟲ posAngle)
        )
    }

  // Shape for the patrol field
  def patrolField(
    posAngle: Angle,
    mode:     Option[ScienceMode],
    port:     PortDisposition
  ): Option[ShapeExpression] =
    mode match {
      case Some(m: ScienceMode.GmosNorthLongSlit) =>
        gmos.probeArm.patrolFieldAt(posAngle, Offset.Zero, m.fpu.asLeft.some, port).some
      case Some(m: ScienceMode.GmosSouthLongSlit) =>
        gmos.probeArm.patrolFieldAt(posAngle, Offset.Zero, m.fpu.asRight.some, port).some
      case _                                      =>
        none
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
    mode match
      case Some(m: ScienceMode.GmosNorthLongSlit) =>
        NonEmptyMap.of(
          (Css("gmos-probe-arm") |+| extraCss,
           gmos.probeArm.shapeAt(posAngle, guideStarOffset, offsetPos, m.fpu.asLeft.some, port)
          )
        )
      case Some(m: ScienceMode.GmosSouthLongSlit) =>
        NonEmptyMap.of(
          (Css("gmos-probe-arm") |+| extraCss,
           gmos.probeArm.shapeAt(posAngle, guideStarOffset, offsetPos, m.fpu.asRight.some, port)
          )
        )
      case _                                      =>
        NonEmptyMap.of(
          (Css("gmos-science-ccd"), gmos.scienceArea.imaging ⟲ posAngle)
        )
