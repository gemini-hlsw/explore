// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.visualization

import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.vdom.svg_<^.*
import lucuma.react.common.Style
import lucuma.react.floatingui.*
import org.scalajs.dom.svg.SVG

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

private val heightPaddingFactor = 1.05
private val widthPaddingFactor  = 1.2
val TooltipScaleFactor          = 1.4

def tooltipTranslationAndContent(
  floating: UseFloatingReturn,
  q:        Double,
  sx:       Double,
  sy:       Double,
  tooltip:  String
): (Double, Double, Double, String, Double) = {
  val (textWidth, textHeight) = textDomSize(tooltip)
  tooltipTranslationAndContent(floating, q, sx, sy, textWidth, textHeight)
}

def tooltipTranslationAndContent(
  floating:   UseFloatingReturn,
  q:          Double,
  sx:         Double,
  sy:         Double,
  textWidth:  Double,
  textHeight: Double
): (Double, Double, Double, String, Double) = {
  val pf = sx.max(sy)

  val tooltipWidth  = textWidth / pf
  val tooltipHeight = textHeight / pf
  val tooltipOffset = tooltipHeight * 0.1;

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

def floatingArea(p: Double, q: Double, areaSize: Double, refs: FloatingRefs) =
  <.rect(
    ^.x      := scale(p) - areaSize / 2,
    ^.width  := areaSize,
    ^.height := areaSize,
    ^.y      := scale(q) - areaSize / 2,
    ^.untypedRef(refs.setReference),
    ExploreStyles.TargetTooltipArea
  )

def svgWithTooltip(
  p:         Double,
  q:         Double,
  sx:        Double,
  sy:        Double,
  open:      TooltipState,
  tooltip:   String,
  floating:  UseFloatingReturn,
  targetSvg: VdomElement
) =
  if (open.value) {

    val (translateBoxY, translateTextX, translateTextY, path, pf) =
      tooltipTranslationAndContent(floating, q, sx, sy, tooltip)

    <.g(
      targetSvg,
      <.g(
        ExploreStyles.TargetTooltip,
        ^.transform := s"translate(${scale(p)}, ${translateBoxY}) scale($TooltipScaleFactor, $TooltipScaleFactor)",
        <.path(
          ^.untypedRef(floating.refs.setFloating),
          ^.d         := path
        ),
        <.text(
          ^.transform := s"translate($translateTextX, $translateTextY) scale(${1 / pf}, ${1 / pf})",
          tooltip
        )
      )
    )
  } else {
    targetSvg
  }
