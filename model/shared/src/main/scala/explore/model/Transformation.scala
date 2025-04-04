// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import lucuma.core.math.Angle
import lucuma.core.util.NewBoolean
import monocle.Focus
import monocle.Lens
import monocle.Prism

object ColorsInverted extends NewBoolean { inline def Yes = True; inline def No = False }
type ColorsInverted = ColorsInverted.Type

sealed trait ChartOp derives Eq

object ChartOp:
  case class Rotate(deg: Int)          extends ChartOp
  case class ScaleX(scale: BigDecimal) extends ChartOp
  case class ScaleY(scale: BigDecimal) extends ChartOp
  case class FlipY(flip: Boolean)      extends ChartOp
  case class FlipX(flip: Boolean)      extends ChartOp

  val rotate: Lens[Rotate, Int] =
    Focus[Rotate](_.deg)

  val scaleX: Lens[ScaleX, BigDecimal] =
    Focus[ScaleX](_.scale)

  val scaleY: Lens[ScaleY, BigDecimal] =
    Focus[ScaleY](_.scale)

  val opScaleX: Prism[ChartOp, BigDecimal] =
    Prism[ChartOp, BigDecimal] {
      case ScaleX(x) => Some(x)
      case _         => None
    }(ScaleX(_))

case class Transformation(
  rotate:   ChartOp.Rotate,
  scaleX:   ChartOp.ScaleX,
  scaleY:   ChartOp.ScaleY,
  flipX:    ChartOp.FlipX,
  flipY:    ChartOp.FlipY,
  inverted: ColorsInverted
) derives Eq:
  private def normalizeFlips: Transformation =
    val p1 = if flipX.flip then copy(scaleX = ChartOp.ScaleX(-1 * this.scaleX.scale)) else this
    val p2 = if flipY.flip then p1.copy(scaleY = ChartOp.ScaleY(-1 * this.scaleY.scale)) else p1
    p2

  def calcTransform: List[String] =
    val p = normalizeFlips
    List(p.rotate, p.scaleX, p.scaleY)
      .foldLeft(List.empty[String]) { (acc, op) =>
        op match {
          case ChartOp.ScaleX(x) => s"scaleX(${x})" :: acc
          case ChartOp.ScaleY(y) => s"scaleY(${y})" :: acc
          case ChartOp.Rotate(x) => s"rotate(${x}deg)" :: acc
          case _                 => acc
        }
      }
      .reverse

  inline def flip: Transformation =
    copy(flipX = ChartOp.FlipX(!this.flipX.flip))

  inline def rotateLeft: Transformation =
    copy(rotate = ChartOp.Rotate((this.rotate.deg - 90) % 360))

  inline def rotateRight: Transformation =
    copy(rotate = ChartOp.Rotate((this.rotate.deg + 90) % 360))

  inline def rotateTo(angle: Angle): Transformation =
    copy(rotate = ChartOp.Rotate(angle.toSignedDoubleDegrees.toInt))

  inline def vflip: Transformation =
    copy(flipY = ChartOp.FlipY(!this.flipY.flip))

  inline def zoomOut: Transformation =
    copy(scaleX = ChartOp.ScaleX((this.scaleX.scale * 8) / 10),
         scaleY = ChartOp.ScaleY((this.scaleY.scale * 8) / 10)
    )

  inline def zoomIn: Transformation =
    copy(scaleX = ChartOp.ScaleX((this.scaleX.scale * 12) / 10),
         scaleY = ChartOp.ScaleY((this.scaleY.scale * 12) / 10)
    )

  inline def reset: Transformation = Transformation.Default

object Transformation:
  val Default = Transformation(ChartOp.Rotate(0),
                               ChartOp.ScaleX(1),
                               ChartOp.ScaleY(1),
                               ChartOp.FlipX(false),
                               ChartOp.FlipY(false),
                               ColorsInverted.No
  )

  val rotate: Lens[Transformation, ChartOp.Rotate] =
    Focus[Transformation](_.rotate)

  val scaleX: Lens[Transformation, ChartOp.ScaleX] =
    Focus[Transformation](_.scaleX)

  val scaleY: Lens[Transformation, ChartOp.ScaleY] =
    Focus[Transformation](_.scaleY)

  val inverted: Lens[Transformation, ColorsInverted] =
    Focus[Transformation](_.inverted)

  val rotateDeg = rotate.andThen(ChartOp.rotate)
  val scaleXVal = scaleX.andThen(ChartOp.scaleX)
  val scaleYVal = scaleY.andThen(ChartOp.scaleY)
