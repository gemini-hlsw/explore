// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.all._
import crystal.react.View
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.enums.AgsState
import explore.model.enums.Visible
import explore.model.formats._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ags.GuideStarCandidate
import lucuma.core.math._
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.aladin.Fov
import react.common.ReactFnProps
import react.fa.Transform
import react.floatingui.*
import react.semanticui.elements.button.Button
import react.semanticui.elements.label._
import react.semanticui.shorthand._
import react.semanticui.sizes.Small
import react.semanticui.sizes._

final case class AladinToolbar(
  fov:               Fov,
  current:           Coordinates,
  agsState:          AgsState,
  selectedGuideStar: Option[GuideStarCandidate],
  center:            View[Boolean],
  agsOverlay:        Visible
) extends ReactFnProps[AladinToolbar](AladinToolbar.component)

object AladinToolbar {
  type Props = AladinToolbar

  val component =
    ScalaFnComponent[Props] { props =>
      React.Fragment(
        Label(
          icon = Icons.Maximize.clazz(ExploreStyles.Accented),
          clazz = ExploreStyles.AladinFOV,
          size = Small,
          detail = LabelDetail(clazz = ExploreStyles.AladinDetailText,
                               content =
                                 s"${formatFov(props.fov.x)} \u00D7 ${formatFov(props.fov.y)}"
          )
        ),
        <.div(
          ExploreStyles.AladinGuideStarLoading,
          Tooltip(
            trigger = <.span(Icons.CircleSmall.beat().clazz(ExploreStyles.WarningIcon)),
            tooltip = "Loading catalog stars.."
          ).when(props.agsState === AgsState.LoadingCandidates),
          Tooltip(
            trigger = <.span(Icons.CircleSmall.beat().clazz(ExploreStyles.WarningIcon)),
            tooltip = "Calculating guide star.."
          ).when(props.agsState === AgsState.Calculating),
          Tooltip(
            trigger = <.span(Icons.CircleSmall.clazz(ExploreStyles.ErrorIcon)),
            tooltip = "The Catalog isn't responding at the moment - please try again later.."
          ).when(props.agsState === AgsState.Error)
        ),
        <.div(
          ExploreStyles.AladinGuideStar,
          props.selectedGuideStar
            .map { case g => s"GS: ${g.name.value}" }
            .unless(props.agsOverlay.visible),
          "Calculating...".when(props.agsState === AgsState.Calculating)
        ),
        Label(
          icon = Icons.MousePointer.clazz(ExploreStyles.Accented),
          clazz = ExploreStyles.AladinCurrentCoords,
          size = Small,
          detail = LabelDetail(clazz = ExploreStyles.AladinDetailText,
                               content = formatCoordinates(props.current)
          )
        ),
        <.div(
          ExploreStyles.AladinCenterButton,
          Tooltip(
            trigger = <.span(
              Button(size = Mini, icon = true, onClick = props.center.set(true))(
                Icons.Bullseye
                  .transform(Transform(size = 24))
                  .clazz(ExploreStyles.Accented)
              )
            ),
            tooltip = "Center on target",
            placement = Placement.BottomEnd
          )
        )
      )
    }
}
