// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.visualization

import eu.timepit.refined.types.numeric.NonNegInt
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.TagOf
import japgolly.scalajs.react.vdom.svg_<^.*
import lucuma.core.enums.SequenceType
import lucuma.core.math.Offset
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.tooltip.*
import lucuma.ui.syntax.all.given
import org.scalajs.dom.SVGGElement

case class OffsetSVG(
  p:        Double,
  q:        Double,
  maxP:     Long,
  radius:   Double,
  pointCss: Css,
  oType:    SequenceType,
  idx:      NonNegInt,
  offset:   Offset
) extends ReactFnProps(OffsetSVG.component)

object OffsetSVG:
  private type Props = OffsetSVG

  private val component =
    ScalaFnComponent[Props]: p =>
      val areaSize = scale(p.maxP * (2 * p.radius + 3))

      val (offP, offQ) = Offset.signedDecimalArcseconds.get(p.offset)
      val prefix       = p.oType match
        case SequenceType.Science     => "Sci. Offset"
        case SequenceType.Acquisition => "Acq. Offset"

      val tooltip = f"$prefix: ${p.idx}%s, p=${offP}%.0f, q=${offQ}%.0f"

      <.g(ExploreStyles.VisualizationTooltipTarget)(
        <.rect( // mark the offsset pos with a square
          ^.x      := scale(p.p) - areaSize / 2,
          ^.width  := areaSize,
          ^.height := areaSize,
          ^.y      := scale(p.q) - areaSize / 2,
          p.pointCss
        )
      ).withTooltipOptions(content = tooltip)
