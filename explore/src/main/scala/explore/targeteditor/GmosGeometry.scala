// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.data.NonEmptyList
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all.*
import explore.model.ObsConfiguration
import lucuma.ags.AgsAnalysis
import lucuma.core.enums.PortDisposition
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.gmos
import lucuma.core.geom.syntax.shapeexpression.*
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.react.common.style.Css
import lucuma.schemas.model.BasicConfiguration

import scala.collection.immutable.SortedMap

/**
 * Test object to produce a gmos geometry. it is for demo purposes only
 */
object GmosGeometry:

  // Shape to display for a specific mode
  def shapesForMode(
    posAngle:      Angle,
    offset:        Offset,
    configuration: Option[BasicConfiguration],
    port:          PortDisposition
  ): SortedMap[Css, ShapeExpression] =
    configuration match {
      case Some(m: BasicConfiguration.GmosNorthLongSlit) =>
        SortedMap(
          (Css("gmos-science-ccd"), gmos.scienceArea.imaging ⟲ posAngle),
          (Css("gmos-fpu"), gmos.scienceArea.shapeAt(posAngle, offset, m.fpu.asLeft.some)),
          (Css("gmos-patrol-field"),
           gmos.probeArm.patrolFieldAt(posAngle, offset, m.fpu.asLeft.some, port)
          )
        )
      case Some(m: BasicConfiguration.GmosSouthLongSlit) =>
        SortedMap(
          (Css("gmos-science-ccd"), gmos.scienceArea.imaging ⟲ posAngle),
          (Css("gmos-fpu"), gmos.scienceArea.shapeAt(posAngle, offset, m.fpu.asRight.some)),
          (Css("gmos-patrol-field"),
           gmos.probeArm.patrolFieldAt(posAngle, offset, m.fpu.asRight.some, port)
          )
        )
      case _                                             =>
        SortedMap(
          (Css("gmos-science-ccd"), gmos.scienceArea.imaging ⟲ posAngle)
        )
    }

  // Shape for the intersection of patrol fields at each offset
  def patrolFieldIntersection(
    posAngle:      Angle,
    offsets:       NonEmptyList[Offset],
    configuration: BasicConfiguration,
    port:          PortDisposition,
    extraCss:      Css = Css.Empty
  ): (Css, ShapeExpression) =
    (Css("patrol-field-intersection") |+| extraCss) ->
      offsets
        .map(patrolField(posAngle, _, configuration, port))
        .reduce(_ ∩ _)

  // Shape for the patrol field at a single position
  def patrolField(
    posAngle:      Angle,
    offset:        Offset,
    configuration: BasicConfiguration,
    port:          PortDisposition
  ): ShapeExpression =
    configuration match {
      case m: BasicConfiguration.GmosNorthLongSlit =>
        gmos.probeArm.patrolFieldAt(posAngle, offset, m.fpu.asLeft.some, port)
      case m: BasicConfiguration.GmosSouthLongSlit =>
        gmos.probeArm.patrolFieldAt(posAngle, offset, m.fpu.asRight.some, port)
    }

  // Shape to display always
  def commonShapes(posAngle: Angle, extraCss: Css): SortedMap[Css, ShapeExpression] =
    SortedMap(
      (Css("gmos-candidates-area") |+| extraCss,
       gmos.probeArm.candidatesAreaAt(posAngle, Offset.Zero)
      )
    )

  // Shape to display always
  def probeShapes(
    posAngle:        Angle,
    guideStarOffset: Offset,
    offsetPos:       Offset,
    mode:            Option[BasicConfiguration],
    port:            PortDisposition,
    extraCss:        Css
  ): SortedMap[Css, ShapeExpression] =
    mode match
      case Some(m: BasicConfiguration.GmosNorthLongSlit) =>
        SortedMap(
          (Css("gmos-probe-arm") |+| extraCss,
           gmos.probeArm.shapeAt(posAngle, guideStarOffset, offsetPos, m.fpu.asLeft.some, port)
          )
        )
      case Some(m: BasicConfiguration.GmosSouthLongSlit) =>
        SortedMap(
          (Css("gmos-probe-arm") |+| extraCss,
           gmos.probeArm.shapeAt(posAngle, guideStarOffset, offsetPos, m.fpu.asRight.some, port)
          )
        )
      case _                                             =>
        SortedMap(
          (Css("gmos-science-ccd"), gmos.scienceArea.imaging ⟲ posAngle)
        )

  // Full geometry for GMOS
  def gmosGeometry(
    referenceCoordinates:    Coordinates,
    configuration:           Option[ObsConfiguration],
    port:                    PortDisposition,
    gs:                      Option[AgsAnalysis.Usable],
    candidatesVisibilityCss: Css
  ): Option[SortedMap[Css, ShapeExpression]] =
    gs.map(_.posAngle)
      .orElse(configuration.flatMap(_.fallbackPosAngle))
      .map { posAngle =>
        val basicConf = configuration.flatMap(_.configuration)

        // Shapes at base position
        val baseShapes: SortedMap[Css, ShapeExpression] =
          GmosGeometry.shapesForMode(posAngle, Offset.Zero, basicConf, port) ++
            GmosGeometry.commonShapes(posAngle, candidatesVisibilityCss)

        // Don't show the probe if there is no usable GS
        baseShapes ++ gs
          .map { gs =>
            val gsOffset   =
              referenceCoordinates.diff(gs.target.tracking.baseCoordinates).offset
            val probeShape =
              GmosGeometry.probeShapes(posAngle, gsOffset, Offset.Zero, basicConf, port, Css.Empty)

            val offsets =
              (configuration.flatMap(_.scienceOffsets) |+|
                configuration.flatMap(_.acquisitionOffsets))
                .orElse(NonEmptyList.one(Offset.Zero).some)

            val patrolFieldIntersection =
              for {
                conf <- basicConf
                o    <- offsets
              } yield GmosGeometry
                .patrolFieldIntersection(posAngle, o.distinct, conf, port)

            patrolFieldIntersection.fold(probeShape)(probeShape + _)
          }
          .getOrElse(SortedMap.empty[Css, ShapeExpression])
      }
