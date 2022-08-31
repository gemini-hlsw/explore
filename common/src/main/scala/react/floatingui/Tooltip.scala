// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package react.floatingui

import cats.syntax.all.*
import japgolly.scalajs.react._
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.TagOf
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import org.scalajs.dom
import org.scalajs.dom.html
import react.common.ReactFnProps
import react.common.Style
import react.floatingui.hooks._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.|

import js.annotation._

/**
 * Tooltip base on floating ui see: https://floating-ui.com/docs/react-dom
 */
case class Tooltip(trigger: VdomTag, tooltip: VdomNode, placement: Placement = Placement.Top)
    extends ReactFnProps(Tooltip.component)

object Tooltip {
  private type Props = Tooltip

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useState(false) // isOpen
      .useRefToVdom[dom.HTMLElement] // arrow
      .useFloatingBy { (props, open, arrow) =>
        UseFloatingProps(
          placement = props.placement,
          open = open.value,
          onOpenChange = open.setState,
          middleware = List(
            middleware.flip(),
            middleware.shift(ShiftOptions(padding = 5)),
            middleware.offset(4),
            middleware.arrow(ArrowElement(arrow.raw))
          )
        )
      }
      .useInteractionsBy { (_, _, _, h) =>
        List(middleware.useHover(h.context))
      }
      .render { (props, open, arrow, floating, _) =>
        val display: Map[String, String | Int] =
          if (open.value) Map.empty[String, String | Int]
          else Map[String, String | Int]("display" -> "none")

        val style = (floating.x.toOption, floating.y.toOption) match {
          case (Some(x), Some(y)) =>
            Style(
              Map(
                "position" -> floating.strategy,
                "left"     -> s"${x}px",
                "top"      -> s"${y}px"
              ) ++ display
            )
          case _                  =>
            Style(
              Map(
                "position" -> floating.strategy,
                "left"     -> "0",
                "top"      -> "0"
              ) ++ display
            )
        }

        val arrowOpt                                 = floating.middlewareData.arrow.toOption
        val arrowStyleMap: Map[String, String | Int] =
          (arrowOpt.flatMap(_.x.toOption), arrowOpt.flatMap(_.y.toOption)) match {
            case (Some(x), Some(y)) =>
              Map("left" -> s"${x}px", "top" -> s"${y}px", "right" -> "", "bottom" -> "")
            case (Some(x), None)    =>
              Map("left" -> s"${x}px", "top" -> s"", "right" -> "", "bottom" -> "")
            case (None, Some(y))    =>
              Map("left" -> "", "top" -> s"${y}px", "right" -> "", "bottom" -> "")
            case _                  => Map.empty
          }

        val arrowShift  = "-4px"
        val arrowBorder = "var(--tooltip-border-width)"

        val placementStyle: Map[String, String | Int] =
          Placement.fromString(floating.placement) match {
            case Some(Placement.Top) | Some(Placement.TopStart) | Some(Placement.TopEnd)          =>
              Map("bottom"              -> arrowShift,
                  "border-right-width"  -> arrowBorder,
                  "border-bottom-width" -> arrowBorder
              )
            case Some(Placement.Bottom) | Some(Placement.BottomStart) | Some(Placement.BottomEnd) =>
              Map("top"               -> arrowShift,
                  "border-left-width" -> arrowBorder,
                  "border-top-width"  -> arrowBorder
              )
            case Some(Placement.Left) | Some(Placement.LeftStart) | Some(Placement.LeftEnd)       =>
              Map("right"              -> arrowShift,
                  "border-right-width" -> arrowBorder,
                  "border-top-width"   -> arrowBorder
              )
            case Some(Placement.Right) | Some(Placement.RightStart) | Some(Placement.RightEnd)    =>
              Map("left"                -> arrowShift,
                  "border-left-width"   -> arrowBorder,
                  "border-bottom-width" -> arrowBorder
              )
            case _                                                                                => Map.empty
          }

        val arrowStyle =
          arrowOpt.fold(Style(display))(_ => Style(display ++ arrowStyleMap ++ placementStyle))
        ReactFragment(
          props.trigger(^.untypedRef := floating.reference),
          if (open.value)
            <.div(
              ^.untypedRef := floating.floating,
              ^.cls        := "tooltip",
              ^.style      := style.toJsObject,
              props.tooltip,
              <.div(^.cls := "arrow", ^.untypedRef := arrow, ^.style := arrowStyle.toJsObject)
            )
          else
            EmptyVdom
        )
      }
}
