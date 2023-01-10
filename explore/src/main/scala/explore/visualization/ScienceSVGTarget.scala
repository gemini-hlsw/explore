// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.visualization

import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.svg_<^.*
import lucuma.ui.syntax.all.given
import org.scalajs.dom.svg.SVG
import react.common.Css
import react.common.ReactFnProps
import react.common.Style
import react.floatingui.*
import react.floatingui.hooks.*

case class CrossTarget(
  svg:         Option[SVG],
  p:           Double,
  q:           Double,
  maxP:        Long,
  radius:      Double,
  pointCss:    Css,
  selectedCss: Css,
  sx:          Double,
  sy:          Double,
  selected:    Boolean,
  title:       Option[String]
) extends ReactFnProps(CrossTarget.component)

object CrossTarget {
  type Props = CrossTarget

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useState(TooltipState.Closed) // isOpen
      .useFloatingBy { (_, open) =>
        UseFloatingProps(
          placement = Placement.Top,
          open = open.value.value,
          onOpenChange = s => open.setState(TooltipState(s)),
          middleware = List(
            middleware.flip()
          )
        )
      }
      .useInteractionsBy { (_, _, h) =>
        List(middleware.useHover(h.context))
      }
      .render { (p, open, floating, _) =>
        val display: Map[String, String | Int] =
          if (open.value.value) Map.empty[String, String | Int]
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

        val side      = scale(p.maxP * p.radius)
        val areaSize  = scale(p.maxP * (2 * p.radius + 3))
        val targetSvg = <.g(
          <.rect(
            ^.x          := scale(p.p) - areaSize / 2,
            ^.width      := areaSize,
            ^.height     := areaSize,
            ^.y          := scale(p.q) - areaSize / 2,
            ^.untypedRef := floating.reference,
            ExploreStyles.TargetTooltipArea
          ),
          <.circle(
            ^.cx := scale(p.p),
            ^.cy := scale(p.q),
            ^.r  := scale(p.maxP * (p.radius + 3)),
            p.selectedCss
          ).when(p.selected),
          <.circle(
            ^.cx         := scale(p.p),
            ^.cy         := scale(p.q),
            ^.r          := scale(p.maxP * p.radius),
            p.pointCss
          )
        )

        val tooltipPos = p.svg.map { svg =>
          val ctm       = svg.getScreenCTM()
          val pt        = svg.createSVGPoint()
          pt.x = scale(p.p)
          pt.y = scale(p.q)
          val tooltipPt = pt.matrixTransform(ctm.inverse())
          (tooltipPt.x, tooltipPt.y)
        }

        val tooltip             = p.title.getOrElse("<>")
        val heightPaddingFactor = 1.05
        val widthPaddingFactor  = 1.2

        val pf = p.sx.max(p.sy)

        val (textWidth, textHeight) = textDomSize(tooltip)
        val tooltipWidth            = textWidth / pf
        val tooltipHeight           = textHeight / pf
        val tooltipOffset           = tooltipHeight * 0.1;

        val translateTextX                        = -tooltipWidth / 2
        val (translateBoxY, translateTextY, path) = Placement.fromString(floating.placement) match
          case Some(Placement.Top) | Some(Placement.TopStart) | Some(Placement.TopEnd) =>
            (scale(p.q) - 2 * tooltipOffset,
             -heightPaddingFactor * (tooltipHeight - tooltipOffset) / 2,
             topTooltipPath(widthPaddingFactor * tooltipWidth,
                            heightPaddingFactor * tooltipHeight,
                            tooltipOffset,
                            tooltipWidth / 15
             )
            )
          case _                                                                       =>
            (scale(p.q) + tooltipOffset,
             (1 + heightPaddingFactor) * (tooltipHeight - tooltipOffset) / 2,
             bottomTooltipPath(widthPaddingFactor * tooltipWidth,
                               heightPaddingFactor * tooltipHeight,
                               tooltipOffset,
                               tooltipWidth / 15
             )
            )

        if (open.value.value) {
          <.g(
            targetSvg,
            <.g(
              ExploreStyles.TargetTooltip,
              ^.transform := s"translate(${scale(p.p)}, ${translateBoxY})",
              <.path(
                ^.untypedRef := floating.floating,
                ^.d          := path
              ),
              <.text(
                ^.transform  := s"translate($translateTextX, $translateTextY) scale(${1 / pf}, ${1 / pf})",
                tooltip
              )
            )
          )
        } else {
          targetSvg
        }
      }
}
