// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.visualization

import cats.syntax.all.*
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.svg_<^.*
import lucuma.ags.AgsAnalysis
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.ui.syntax.all.given

case class GuideStarTarget(
  offP:     Double,
  offQ:     Double,
  maxP:     Long,
  radius:   Double,
  pointCss: Css,
  analysis: AgsAnalysis
) extends ReactFnProps(GuideStarTarget.component)

object GuideStarTarget:
  private type Props = GuideStarTarget

  private val component =
    ScalaFnComponent[Props]: p =>
      val pointCss: Css =
        ExploreStyles.GuideStarCandidateTarget |+| p.pointCss

      <.circle(ExploreStyles.VisualizationTooltipTarget |+| p.analysis.target.selector)(
        ^.cx := scale(p.offP),
        ^.cy := scale(p.offQ),
        ^.r  := scale(p.maxP * p.radius),
        pointCss
      )
