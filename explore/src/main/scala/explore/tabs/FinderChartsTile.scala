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
import explore.utils.*

sealed trait ChartOp derives Eq

object ChartOp:
  case class Rotate(deg: Int)      extends ChartOp
  case class ScaleX(scale: Double) extends ChartOp
  case class ScaleY(scale: Double) extends ChartOp

  def calcTransform(ops: List[ChartOp]): List[String] =
    ops
      .foldLeft(List.empty[String]) { (acc, op) =>
        op match {
          case ScaleX(x) => s"scaleX($x)" :: acc
          case ScaleY(y) => s"scaleY($y)" :: acc
          case Rotate(x) => s"rotate(${x}deg)" :: acc
        }
      }
      .reverse

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

  val DefaultOps = List[ChartOp](ChartOp.ScaleX(1), ChartOp.ScaleY(1), ChartOp.Rotate(0))

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateView(DefaultOps)
      .useStateView(ColorsInverted.No)
      .render { (_, ops, inverted) =>
        val transforms = ChartOp.calcTransform(ops.get)
        println(ops.get)
        println(inverted.get)
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

case class FinderChartsControlOverlay(ops: View[List[ChartOp]], inverted: View[ColorsInverted])
    extends ReactFnProps[FinderChartsControlOverlay](FinderChartsControlOverlay.component)

object FinderChartsControlOverlay {
  type Props = FinderChartsControlOverlay

  extension (ops: List[ChartOp])
    inline def flip: List[ChartOp] =
      ops.collect {
        case ChartOp.ScaleX(x) => ChartOp.ScaleX(-1 * x)
        case l                 => l
      }

    inline def rotateLeft: List[ChartOp] =
      ops.collect {
        case ChartOp.Rotate(deg) => ChartOp.Rotate(deg - 90)
        case l                   => l
      }

    inline def rotateRight: List[ChartOp] =
      ops.collect {
        case ChartOp.Rotate(deg) => ChartOp.Rotate(deg + 90)
        case l                   => l
      }

    inline def vflip: List[ChartOp] =
      ops.collect {
        case ChartOp.ScaleY(x) => ChartOp.ScaleY(-1 * x)
        case l                 => l
      }

    inline def zoomOut: List[ChartOp] =
      ops.collect {
        case ChartOp.ScaleY(x) => ChartOp.ScaleY(x * 0.8)
        case ChartOp.ScaleX(x) => ChartOp.ScaleX(x * 0.8)
        case l                 => l
      }

    inline def zoomIn: List[ChartOp] =
      ops.collect {
        case ChartOp.ScaleY(x) => ChartOp.ScaleY(x * 1.2)
        case ChartOp.ScaleX(x) => ChartOp.ScaleX(x * 1.2)
        case l                 => l
      }

    inline def reset: List[ChartOp] = FinderCharts.DefaultOps

  val component =
    ScalaFnComponent[Props] { p =>
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
          <.div(ExploreStyles.FinderChartsButton,
                ^.onClick --> p.ops.mod(_.rotateLeft),
                Icons.ArrowRotateLeft.withBorder(true).withFixedWidth(true)
          ),
          <.div("Rotate"),
          <.div(ExploreStyles.FinderChartsButton,
                ^.onClick --> p.ops.mod(_.rotateRight),
                Icons.ArrowRotateRight.withBorder(true).withFixedWidth(true)
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
          <.div(),
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
