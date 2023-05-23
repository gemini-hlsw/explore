// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.all.*
import crystal.react.ViewOpt
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.Constants
import explore.model.TargetVisualOptions
import explore.model.enums.AgsState
import explore.model.enums.Visible
import explore.model.formats.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ags.AgsAnalysis
import lucuma.ags.GuideStarCandidate
import lucuma.core.math.*
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.aladin.Fov
import react.common.ReactFnProps
import react.fa.Transform
import react.floatingui.Placement
import react.floatingui.syntax.*
import react.primereact.Button

case class AladinToolbar(
  fov:               Fov,
  current:           Coordinates,
  agsState:          AgsState,
  selectedGuideStar: Option[AgsAnalysis],
  agsOverlay:        Visible,
  viewOffset:        ViewOpt[Offset]
) extends ReactFnProps(AladinToolbar.component)

object AladinToolbar {
  private type Props = AladinToolbar

  private val component =
    ScalaFnComponent[Props] { props =>
      val usableGuideStar = props.selectedGuideStar.forall(_.isUsable)

      React.Fragment(
        <.label(
          Icons.Maximize.withClass(ExploreStyles.Accented),
          ExploreStyles.AladinFOV,
          <.span(ExploreStyles.AladinDetailText,
                 s"${formatFov(props.fov.x)} \u00D7 ${formatFov(props.fov.y)}"
          )
        ),
        <.div(
          ExploreStyles.AladinGuideStarLoading,
          <.span(Icons.CircleSmall.withBeat().withClass(ExploreStyles.WarningIcon))
            .withTooltip(
              tooltip = "Loading catalog stars.."
            )
            .when(props.agsState === AgsState.LoadingCandidates),
          <.span(Icons.CircleSmall.withBeat().withClass(ExploreStyles.WarningIcon))
            .withTooltip(
              tooltip = "Calculating guide star.."
            )
            .when(props.agsState === AgsState.Calculating),
          <.span(Icons.CircleSmall.withClass(ExploreStyles.ErrorIcon))
            .withTooltip(
              tooltip = "The Catalog isn't responding at the moment - please try again later.."
            )
            .when(props.agsState === AgsState.Error)
        ),
        <.div(
          ExploreStyles.AladinGuideStar,
          props.selectedGuideStar
            .map { case g => s"GS: ${g.target.name.value}" }
            .unless(props.agsOverlay.isVisible || !usableGuideStar),
          Constants.NoGuideStarMessage.when(!usableGuideStar && props.agsState === AgsState.Idle),
          "Calculating...".when(props.agsState === AgsState.Calculating)
        ),
        <.label(
          Icons.MousePointer.withClass(ExploreStyles.Accented),
          ExploreStyles.AladinCurrentCoords,
          <.span(ExploreStyles.AladinDetailText, formatCoordinates(props.current))
        ),
        <.div(
          ExploreStyles.AladinCenterButton,
          <.span(
            Button(
              text = true,
              onClick = props.viewOffset.set(Offset.Zero),
              icon = Icons.Bullseye
                .withTransform(Transform(size = 24))
                .withClass(ExploreStyles.Accented)
            ).compact.small
          ).withTooltip(
            tooltip = "Center on target",
            placement = Placement.BottomEnd
          )
        )
      )
    }
}
