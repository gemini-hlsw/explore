// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.findercharts

import cats.Eq
import cats.derived.*
import crystal.react.hooks.*
import explore.Resources
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ui.syntax.all.given
import monocle.Focus
import monocle.Lens
import monocle.Prism
import react.common.ReactFnProps
import explore.model.ObsAttachmentList
import lucuma.core.model.Program
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import scala.collection.immutable.SortedSet
import lucuma.core.model.ObsAttachment

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
  rotate: ChartOp.Rotate,
  scaleX: ChartOp.ScaleX,
  scaleY: ChartOp.ScaleY,
  flipX:  ChartOp.FlipX,
  flipY:  ChartOp.FlipY
):
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
                               ChartOp.FlipY(false)
  )

  val rotate: Lens[Transformation, ChartOp.Rotate] =
    Focus[Transformation](_.rotate)

  val scaleX: Lens[Transformation, ChartOp.ScaleX] =
    Focus[Transformation](_.scaleX)

  val scaleY: Lens[Transformation, ChartOp.ScaleY] =
    Focus[Transformation](_.scaleY)

  val rotateDeg = rotate.andThen(ChartOp.rotate)
  val scaleXVal = scaleX.andThen(ChartOp.scaleX)
  val scaleYVal = scaleY.andThen(ChartOp.scaleY)

case class FinderCharts(
  programId:        Program.Id,
  authToken:        NonEmptyString,
  obsAttachmentIds: View[SortedSet[ObsAttachment.Id]],
  obsAttachments:   View[ObsAttachmentList]
) extends ReactFnProps(FinderCharts.component)

object FinderCharts:
  private type Props = FinderCharts

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateView(Transformation.Default)
      .useStateView(ColorsInverted.No)
      .render { (props, ops, inverted) =>
        val transforms = ops.get.calcTransform
        ReactFragment(
          ControlOverlay(ops, inverted),
          AttachmentsOverlay(props.programId,
                             props.authToken,
                             props.obsAttachmentIds,
                             props.obsAttachments
          ),
          <.div(
            ExploreStyles.FinderChartsBody,
            <.img(
              ExploreStyles.FinderChartsImage,
              ExploreStyles.FinderChartsImageInverted.when(inverted.get.value),
              ^.transform := transforms.mkString(" "),
              ^.src       := Resources.DemoFinderChart1
            )
          )
        )
      }
