// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.visualization

import cats.syntax.all.*
import explore.Icons
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ags.AgsAnalysis
import lucuma.core.enums.Band
import lucuma.core.enums.GuideSpeed
import lucuma.react.common.ReactFnProps
import lucuma.ui.syntax.all.given

case class GuideStarTooltip(analysis: AgsAnalysis) extends ReactFnProps(GuideStarTooltip.component)

object GuideStarTooltip:
  private type Props = GuideStarTooltip

  extension (s: String)
    private def toSentenceCase: String =
      s"${s.charAt(0).toUpper}${s.substring(1)}"

  private val component =
    ScalaFnComponent[Props]: p =>
      val id = s"Gaia DR3 ${p.analysis.target.id}"

      def speedIcon(guideSpeed: GuideSpeed) = guideSpeed match
        case GuideSpeed.Fast   =>
          Icons.CircleSmall.withClass(ExploreStyles.AgsFast)
        case GuideSpeed.Medium =>
          Icons.CircleSmall.withClass(ExploreStyles.AgsMedium)
        case GuideSpeed.Slow   =>
          Icons.CircleSmall.withClass(ExploreStyles.AgsSlow)

      val (guideSpeedIcon, speedText) = p.analysis match
        case AgsAnalysis.Usable(_, _, guideSpeed, _, _)                    =>
          (speedIcon(guideSpeed).some, guideSpeed.tag.toSentenceCase)
        case AgsAnalysis.NotReachableAtPosition(_, _, Some(guideSpeed), _) =>
          (speedIcon(guideSpeed).some, guideSpeed.tag.toSentenceCase)
        case _                                                             =>
          (none, "")

      <.div(ExploreStyles.AgsTooltip)(
        <.div(id),
        <.div(
          p.analysis.target.gBrightness.map:
            case (Band.GaiaRP, v) =>
              React.Fragment("G", <.sub("RP"), f": ${v}%.2f ")
            case (b, v)           => React.Fragment(f"${b.shortName}: ${v}%.2f "),
          guideSpeedIcon,
          speedText
        )
      )
