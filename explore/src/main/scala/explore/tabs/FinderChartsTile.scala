// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.Eq
import cats.syntax.all.*
import cats.derived.*
import crystal.react.hooks.*
import explore.components.Tile
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.vdom.html_<^.*
import japgolly.scalajs.react.*
import explore.Resources
import react.common.ReactFnProps
import lucuma.ui.syntax.all.given
import japgolly.scalajs.react.feature.ReactFragment
import explore.Icons
import react.fa.Rotation
import crystal.react.View
import react.primereact.Divider
import lucuma.ui.primereact.*
import react.primereact.Button
import react.fa.Transform
import lucuma.core.util.NewType
import lucuma.refined.*
import explore.utils.*

import lucuma.ui.primereact.given
import lucuma.core.optics.ValidFormat
import lucuma.ui.input.ChangeAuditor
import lucuma.core.validation.InputValidSplitEpi
import lucuma.core.validation.InputValidWedge
import eu.timepit.refined.types.numeric.PosBigDecimal
import monocle.Traversal
import monocle.Focus
import monocle.Prism
import scala.math.*
import monocle.Lens

sealed trait ChartOp derives Eq

object ChartOp:
  case class Rotate(deg: Int)          extends ChartOp
  case class ScaleX(scale: BigDecimal) extends ChartOp
  case class ScaleY(scale: BigDecimal) extends ChartOp

  val rotate: Lens[Rotate, Int] =
    Focus[Rotate](_.deg)

  val scaleX: Lens[ScaleX, BigDecimal] =
    Focus[ScaleX](_.scale)

  val scaleY: Lens[ScaleY, BigDecimal] =
    Focus[ScaleY](_.scale)

  extension (self: ChartOp)
    def fold[A](rotate: Int => A, scaleX: BigDecimal => A, scaleY: BigDecimal => A): A =
      self match
        case Rotate(deg)   => rotate(deg)
        case ScaleX(scale) => scaleX(scale)
        case ScaleY(scale) => scaleY(scale)

  val opScaleX: Prism[ChartOp, BigDecimal] =
    Prism[ChartOp, BigDecimal] {
      case ScaleX(x) => Some(x)
      case _         => None
    }(ScaleX(_))

case class Transformation(rotate: ChartOp.Rotate, scaleX: ChartOp.ScaleX, scaleY: ChartOp.ScaleY):
  def calcTransform: List[String] =
    List(rotate, scaleX, scaleY)
      .foldLeft(List.empty[String]) { (acc, op) =>
        op match {
          case ChartOp.ScaleX(x) => s"scaleX(${x})" :: acc
          case ChartOp.ScaleY(y) => s"scaleY(${y})" :: acc
          case ChartOp.Rotate(x) => s"rotate(${x}deg)" :: acc
        }
      }
      .reverse

  inline def flip: Transformation =
    copy(scaleX = ChartOp.ScaleX(-1 * this.scaleX.scale))

  inline def rotateLeft: Transformation =
    copy(rotate = ChartOp.Rotate((this.rotate.deg - 90) % 360))

  inline def rotateRight: Transformation =
    copy(rotate = ChartOp.Rotate((this.rotate.deg + 90) % 360))

  inline def vflip: Transformation =
    copy(scaleY = ChartOp.ScaleY(-1 * this.scaleY.scale))

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
  val Default = Transformation(ChartOp.Rotate(0), ChartOp.ScaleX(1), ChartOp.ScaleY(1))

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
              Icons.ArrowsRetweet.withBorder(true).withFixedWidth(true),
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
