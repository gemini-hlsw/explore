// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.visualization

import eu.timepit.refined.types.numeric.NonNegInt
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.svg_<^.*
import lucuma.core.enums.SequenceType
import lucuma.core.math.Offset
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.react.floatingui.*
import lucuma.react.floatingui.hooks.*
import lucuma.ui.syntax.all.given
import org.scalajs.dom.svg.SVG

case class OffsetSVG(
  svg:      Option[SVG],
  p:        Double,
  q:        Double,
  maxP:     Long,
  radius:   Double,
  pointCss: Css,
  sx:       Double,
  sy:       Double,
  oType:    SequenceType,
  idx:      NonNegInt,
  offset:   Offset
) extends ReactFnProps(OffsetSVG.component)

object OffsetSVG:
  private type Props = OffsetSVG

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
          // tooltip area
          floatingArea(p.p, p.q, areaSize, floating.refs),
          // mark the offsset pos with a square
          <.rect(
            ^.x      := scale(p.p) - areaSize / 2,
            ^.width  := areaSize,
            ^.height := areaSize,
            ^.y      := scale(p.q) - areaSize / 2,
            p.pointCss
          )
        )

        val (offP, offQ) = Offset.signedDecimalArcseconds.get(p.offset)
        val prefix       = p.oType match
          case SequenceType.Science     => "Sci. Offset"
          case SequenceType.Acquisition => "Acq. Offset"

        val tooltip = f"$prefix: ${p.idx}%s, p=${offP}%.0f, q=${offQ}%.0f"
        svgWithTooltip(p.p, p.q, p.sx, p.sy, open.value, tooltip, floating, targetSvg)
