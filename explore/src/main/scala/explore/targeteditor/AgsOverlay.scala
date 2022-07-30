// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.all._
import crystal.react.View
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.formats._
import japgolly.scalajs.react._
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ags.AgsAnalysis
import lucuma.ags.AgsGuideQuality
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.semanticui.elements.button.Button
import react.semanticui.sizes._

import scala.math.BigDecimal.RoundingMode

final case class AgsOverlay(
  selectedGSIndex:   View[Option[Int]],
  maxIndex:          Int,
  selectedGuideStar: Option[AgsAnalysis]
) extends ReactFnProps[AgsOverlay](AgsOverlay.component)

object AgsOverlay {
  type Props = AgsOverlay

  val component =
    ScalaFnComponent[Props] { props =>
      val selectedIndex = props.selectedGSIndex.get

      props.selectedGuideStar
        .map { case analysis =>
          ReactFragment(
            <.div(
              ExploreStyles.AgsDescription,
              Icons.Bahai,
              analysis.target.name.value,
              <.div(
                ExploreStyles.AgsNavigation,
                Button(
                  as = <.a,
                  size = Mini,
                  basic = true,
                  compact = true,
                  disabled = selectedIndex.exists(_ <= 0),
                  onClick = props.selectedGSIndex.mod(_.map(_ - 1)),
                  clazz = ExploreStyles.BlendedButton |+| ExploreStyles.AgsNavigationButton
                )(Icons.ChevronLeft),
                Button(
                  as = <.a,
                  size = Mini,
                  basic = true,
                  compact = true,
                  disabled = selectedIndex.exists(_ >= props.maxIndex - 1),
                  onClick = props.selectedGSIndex.mod(_.map(_ + 1)),
                  clazz = ExploreStyles.BlendedButton |+| ExploreStyles.AgsNavigationButton
                )(Icons.ChevronRight)
              )
            ),
            <.div(
              ExploreStyles.AgsDescription,
              analysis.quality match {
                case AgsGuideQuality.DeliversRequestedIq =>
                  Icons.CircleSmall.clazz(ExploreStyles.AgsGoodIQ)
                case _                                   => ""
              },
              analysis match {
                case AgsAnalysis.Usable(_, _, Some(speed), _, _) =>
                  React.Fragment(
                    <.div(ExploreStyles.AgsGuideSpeed, speed.tag),
                    <.div(ExploreStyles.AgsGBrightness,
                          analysis.target.gBrightness.map(g =>
                            s"G: ${g.setScale(1, RoundingMode.HALF_DOWN).toString()}"
                          )
                    ),
                    <.div(ExploreStyles.AgsCoordinates,
                          s"(${formatCoordinates(analysis.target.tracking.baseCoordinates)})"
                    )
                  )
                case _                                           => EmptyVdom
              }
            )
          )
        }
        .getOrElse(EmptyVdom)
    }
}
