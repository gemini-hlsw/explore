// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.visualization

import eu.timepit.refined.types.numeric.NonNegInt
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.svg_<^.*
import lucuma.core.math.Offset
import lucuma.ui.syntax.all.given
import org.scalajs.dom.svg.SVG
import react.common.Css
import react.common.ReactFnProps
import react.common.Style
import react.floatingui.*
import react.floatingui.hooks.*

case class OffsetSVG(
  svg:      Option[SVG],
  p:        Double,
  q:        Double,
  maxP:     Long,
  radius:   Double,
  pointCss: Css,
  sx:       Double,
  sy:       Double,
  idx:      NonNegInt,
  offset:   Offset
) extends ReactFnProps(OffsetSVG.component)

object OffsetSVG {
  private type Props = OffsetSVG

  private val component =
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
            ^.cx         := scale(p.p),
            ^.cy         := scale(p.q),
            ^.r          := scale(p.maxP * p.radius),
            p.pointCss
          )
        )

        val (offP, offQ) = Offset.signedDecimalArcseconds.get(p.offset)
        val tooltip      = f"Offset: ${p.idx}%s, p=${offP}%.0f, q=${offQ}%.0f"

        val (translateBoxY, translateTextX, translateTextY, path, pf) =
          SVGTooltip.tooltipTranslationAndContent(floating, p.q, p.sx, p.sy, tooltip)

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
