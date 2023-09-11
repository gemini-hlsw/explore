// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.visualization

import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.svg_<^.*
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.react.floatingui.*
import lucuma.react.floatingui.hooks.*
import lucuma.ui.syntax.all.given
import org.scalajs.dom.svg.SVG

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

object CrossTarget:
  private type Props = CrossTarget

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useState(TooltipState.Closed) // isOpen
      .useFloatingBy: (_, open) =>
        UseFloatingProps(
          placement = Placement.Top,
          open = open.value.value,
          onOpenChange = s => open.setState(TooltipState(s)),
          middleware = List(
            middleware.flip()
          )
        )
      .useInteractionsBy: (_, _, h) =>
        List(middleware.useHover(h.context))
      .render: (p, open, floating, _) =>
        val areaSize  = scale(p.maxP * (2 * p.radius + 3))
        val targetSvg = <.g(
          floatingArea(p.p, p.q, areaSize, floating.reference),
          <.circle(
            ^.cx := scale(p.p),
            ^.cy := scale(p.q),
            ^.r  := scale(p.maxP * (p.radius + 3)),
            p.selectedCss
          ).when(p.selected),
          <.circle(
            ^.cx := scale(p.p),
            ^.cy := scale(p.q),
            ^.r  := scale(p.maxP * p.radius),
            p.pointCss
          )
        )

        val tooltip = p.title.getOrElse("<>")
        svgWithTooltip(p.p, p.q, p.sx, p.sy, open.value, tooltip, floating, targetSvg)
