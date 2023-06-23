// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.findercharts

import crystal.react.View
import explore.Icons
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.NewType
import lucuma.core.validation.InputValidSplitEpi
import lucuma.core.validation.InputValidWedge
import lucuma.refined.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.fa.Transform
import react.primereact.Button
import react.primereact.Divider

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

case class ControlOverlay(ops: View[Transformation], inverted: View[ColorsInverted])
    extends ReactFnProps[ControlOverlay](ControlOverlay.component)

object ControlOverlay {
  type Props = ControlOverlay

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
