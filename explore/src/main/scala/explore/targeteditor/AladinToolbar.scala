// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import crystal.react.View
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.enums.Visible
import explore.model.formats._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ags.GuideStarCandidate
import lucuma.core.math._
import react.aladin.Fov
import react.common._
import react.fa.Transform
import react.fa.given
import react.semanticui.elements.button.Button
import react.semanticui.elements.label._
import react.semanticui.modules.popup.Popup
import react.semanticui.modules.popup.PopupPosition
import react.semanticui.shorthand._
import react.semanticui.sizes.Small
import react.semanticui.sizes._

final case class AladinToolbar(
  fov:                 Fov,
  current:             Coordinates,
  loadingGSCandidates: Option[Boolean], // None is an error
  selectedGuideStar:   Option[GuideStarCandidate],
  center:              View[Boolean],
  agsOverlay:          Visible
) extends ReactFnProps[AladinToolbar](AladinToolbar.component)

object AladinToolbar {
  type Props = AladinToolbar
  //
  // Conversion to simplify usage with semantic ui

  import react.semanticui.ShorthandS
  import react.semanticui.elements.icon.Icon
  import react.fa.FontAwesomeIcon

  import scala.scalajs.js.UndefOr

  given Conversion[FontAwesomeIcon, UndefOr[ShorthandS[Icon]]] = _.render
  given Conversion[FontAwesomeIcon, VdomNode]                  = _.render

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
          Popup(
            content = "Loading catalog stars..",
            position = PopupPosition.TopCenter,
            trigger = Icons.CircleSmall.beat().clazz(ExploreStyles.WarningIcon)
          ).when(props.loadingGSCandidates.exists(identity)),
          Popup(
            content = "The Catalog isn't responding at the moment - please try again later..",
            position = PopupPosition.TopCenter,
            trigger = Icons.CircleSmall.clazz(ExploreStyles.ErrorIcon)
          ).when(props.loadingGSCandidates.isEmpty)
        ),
        <.div(
          ExploreStyles.AladinGuideStar,
          props.selectedGuideStar
            .map { case g => s"GS: ${g.name.value}" }
            .unless(props.agsOverlay.visible)
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
          Popup(
            content = "Center on target",
            position = PopupPosition.BottomLeft,
            trigger = Button(size = Mini, icon = true, onClick = props.center.set(true))(
              Icons.Bullseye
                .transform(Transform(size = 24))
                .clazz(ExploreStyles.Accented)
            )
          )
        )
      )
    }
}
