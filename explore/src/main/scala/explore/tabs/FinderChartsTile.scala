// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.Icons
import explore.Resources
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.optics.ValidFormat
import lucuma.core.util.NewType
import lucuma.core.validation.InputValidSplitEpi
import lucuma.core.validation.InputValidWedge
import lucuma.refined.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.Traversal
import react.common.ReactFnProps
import react.fa.Rotation
import react.fa.Transform
import react.primereact.Button
import react.primereact.Divider

import scala.math.*

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

object ColorsInverted extends NewType[Boolean]:
  val No: ColorsInverted  = ColorsInverted(false)
  val Yes: ColorsInverted = ColorsInverted(true)

  extension (self: ColorsInverted)
    def fold[A](no: => A, yes: => A): A =
      self match
        case ColorsInverted.Yes => yes
        case ColorsInverted.No  => no

    def flip: ColorsInverted = fold(ColorsInverted.Yes, ColorsInverted.No)

type ColorsInverted = ColorsInverted.Type

case class FinderCharts() extends ReactFnProps(FinderCharts.component)

object FinderCharts:
  private type Props = FinderCharts

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateView(Transformation.Default)
      .useStateView(ColorsInverted.No)
      .render { (_, ops, inverted) =>
        val transforms = ops.get.calcTransform
        ReactFragment(
          FinderChartsControlOverlay(ops, inverted),
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

case class FinderChartsControlOverlay(ops: View[Transformation], inverted: View[ColorsInverted])
    extends ReactFnProps[FinderChartsControlOverlay](FinderChartsControlOverlay.component)

object FinderChartsControlOverlay {
  type Props = FinderChartsControlOverlay

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .render { p =>
        val rotateDeg = p.ops.zoom(Transformation.rotateDeg)
        val scaleY    = p.ops.zoom(Transformation.scaleYVal)
        val scaleX    =
          p.ops.zoom(Transformation.scaleXVal).withOnMod(x => scaleY.mod(y => y.signum * x))

        ReactFragment(
          <.div(
            ExploreStyles.FinderChartsTools,
            <.span(Icons.Wrench, " Viewer Controls"),
            Divider(),
            <.div(ExploreStyles.FinderChartsButton,
                  ^.onClick --> p.ops.mod(_.zoomOut),
                  Icons.MagnifyingGlassMinus.withBorder(true).withFixedWidth(true)
            ),
            <.div("Zoom"),
            <.div(ExploreStyles.FinderChartsButton,
                  ^.onClick --> p.ops.mod(_.zoomIn),
                  Icons.MagnifyingGlassPlus.withBorder(true).withFixedWidth(true)
            ),
            FormInputTextView("zoom".refined,
                              value = scaleX,
                              validFormat = InputValidWedge.truncatedBigDecimal(2.refined),
                              changeAuditor = ChangeAuditor.accept.decimal(2.refined)
            ),
            <.div(ExploreStyles.FinderChartsButton,
                  ^.onClick --> p.ops.mod(_.rotateLeft),
                  Icons.ArrowRotateLeft.withBorder(true).withFixedWidth(true)
            ),
            <.div("Rotate"),
            <.div(ExploreStyles.FinderChartsButton,
                  ^.onClick --> p.ops.mod(_.rotateRight),
                  Icons.ArrowRotateRight.withBorder(true).withFixedWidth(true)
            ),
            FormInputTextView("rotate".refined,
                              value = rotateDeg,
                              validFormat = InputValidSplitEpi.int,
                              changeAuditor = ChangeAuditor.accept
            ),
            <.div(ExploreStyles.FinderChartsButton,
                  ^.onClick --> p.ops.mod(_.vflip),
                  Icons.ArrowsFromLine.withBorder(true).withFixedWidth(true)
            ),
            <.div("Flip"),
            <.div(
              ExploreStyles.FinderChartsButton,
              ^.onClick --> p.ops.mod(_.flip),
              Icons.ArrowsFromLine
                .withBorder(true)
                .withFixedWidth(true)
                .withTransform(Transform(rotate = 90))
            ),
            <.div(
              ExploreStyles.FinderChartsButton,
              Icons.ArrowsRepeatLight.withBorder(true).withFixedWidth(true),
              ^.onClick --> p.ops.mod(_.reset) *> p.inverted.set(ColorsInverted.No)
            ),
            <.div("Reset"),
            <.div(
              ExploreStyles.FinderChartsButton,
              Icons.CircleHalfStroke
                .withBorder(true)
                .withFixedWidth(true)
                .withTransform(Transform(rotate = p.inverted.get.fold(0, 180))),
              ^.onClick --> p.inverted.mod(_.flip)
            ),
            <.div("Invert")
          )
        )
      }
}

object FinderChartsTile:

  def finderChartsTile =
    Tile(
      ObsTabTilesIds.FinderChartsId.id,
      s"Finder Charts",
      bodyClass = ExploreStyles.FinderChartsTile.some,
      canMinimize = true
    )(_ => FinderCharts())
