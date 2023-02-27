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
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.math.Offset

object SVGTooltip {

  def displayProp(open: TooltipState): Map[String, String | Int] =
    if (open.value) Map.empty[String, String | Int]
    else Map[String, String | Int]("display" -> "none")

  def styleProp(floating: UseFloatingReturn, open: TooltipState) =
    (floating.x.toOption, floating.y.toOption) match {
      case (Some(x), Some(y)) =>
        Style(
          Map(
            "position" -> floating.strategy,
            "left"     -> s"${x}px",
            "top"      -> s"${y}px"
          ) ++ displayProp(open)
        )
      case _                  =>
        Style(
          Map(
            "position" -> floating.strategy,
            "left"     -> "0",
            "top"      -> "0"
          ) ++ displayProp(open)
        )
    }

  def tooltipPosition(svg: Option[SVG], p: Double, q: Double) = svg.map { svg =>
    val ctm       = svg.getScreenCTM()
    val pt        = svg.createSVGPoint()
    pt.x = scale(p)
    pt.y = scale(q)
    val tooltipPt = pt.matrixTransform(ctm.inverse())
    (tooltipPt.x, tooltipPt.y)
  }

  val heightPaddingFactor = 1.05
  val widthPaddingFactor  = 1.2

  def tooltipTranslationAndContent(
    floating: UseFloatingReturn,
    q:        Double,
    sx:       Double,
    sy:       Double,
    tooltip:  String
  ) = {
    val pf = sx.max(sy)

    val (textWidth, textHeight) = textDomSize(tooltip)
    val tooltipWidth            = textWidth / pf
    val tooltipHeight           = textHeight / pf
    val tooltipOffset           = tooltipHeight * 0.1;

    val translateTextX                        = -tooltipWidth / 2
    val (translateBoxY, translateTextY, path) = Placement.fromString(floating.placement) match
      case Some(Placement.Top) | Some(Placement.TopStart) | Some(Placement.TopEnd) =>
        (scale(q) - 2 * tooltipOffset,
         -heightPaddingFactor * (tooltipHeight - tooltipOffset) / 2,
         topTooltipPath(widthPaddingFactor * tooltipWidth,
                        heightPaddingFactor * tooltipHeight,
                        tooltipOffset,
                        tooltipWidth / 15
         )
        )
      case _                                                                       =>
        (scale(q) + tooltipOffset,
         (1 + heightPaddingFactor) * (tooltipHeight - tooltipOffset) / 2,
         bottomTooltipPath(widthPaddingFactor * tooltipWidth,
                           heightPaddingFactor * tooltipHeight,
                           tooltipOffset,
                           tooltipWidth / 15
         )
        )
    (translateBoxY, translateTextX, translateTextY, path, pf)
  }
}
