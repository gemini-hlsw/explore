// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import crystal.react.View
import explore.Icons
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.HourAngle.HMS
import lucuma.core.math._
import react.aladin.Fov
import react.common.ReactFnProps
import react.fa.Transform
import react.semanticui.elements.button.Button
import react.semanticui.elements.label._
import react.semanticui.modules.popup.Popup
import react.semanticui.modules.popup.PopupPosition
import react.semanticui.shorthand._
import react.semanticui.sizes.Small
import react.semanticui.sizes._

import scala.math.rint

final case class AladinToolbar(
  fov:                 Fov,
  current:             Coordinates,
  loadingGSCandidates: Boolean,
  center:              View[Boolean]
) extends ReactFnProps[AladinToolbar](AladinToolbar.component)

object AladinToolbar {
  type Props = AladinToolbar

  // This is used for fov thus needs to be fairly smal
  // implicit val fovReuse: Reusability[Fov]     = exactFovReuse
  // implicit val propsReuse: Reusability[Props] = Reusability.derive

  // TODO: We may want to move these to gsp-math
  def formatHMS(hms: HMS): String =
    f"${hms.hours}%02d:${hms.minutes}%02d:${hms.seconds}%02d.${hms.milliseconds}%03d"

  val fromStringDMS: Angle => String =
    dms => {
      val r = Angle.dms.get(dms)
      f"${r.degrees}%02d:${r.arcminutes}%02d:${r.arcseconds}%02d.${r.milliarcseconds / 10}%02d"
    }

  val fromStringSignedDMS: Angle => String =
    a =>
      if (Angle.signedMicroarcseconds.get(a) < 0) s"-${fromStringDMS(-a)}"
      else s"+${fromStringDMS(a)}"

  def formatCoordinates(coords: Coordinates): String = {
    val ra = HMS(coords.ra.toHourAngle)
    s"${formatHMS(ra)} ${fromStringSignedDMS(coords.dec.toAngle)}"
  }

  def formatFov(angle: Angle): String = {
    val dms        = Angle.DMS(angle)
    val degrees    = dms.degrees
    val arcminutes = dms.arcminutes
    val arcseconds = dms.arcseconds
    val mas        = rint(dms.milliarcseconds.toDouble / 10).toInt
    if (degrees >= 45)
      f"$degrees%02d°"
    else if (degrees >= 1)
      f"$degrees%02d°$arcminutes%02d′"
    else if (arcminutes >= 1)
      f"$arcminutes%02d′$arcseconds%01d″"
    else
      f"$arcseconds%01d.$mas%02d″"
  }

  val component =
    ScalaFnComponent[Props] { props =>
      React.Fragment(
        Label(
          icon = Icons.Maximize.clazz(ExploreStyles.Accented),
          clazz = ExploreStyles.AladinFOV,
          size = Small,
          detail = LabelDetail(
            clazz = ExploreStyles.AladinDetailText,
            content = s"${formatFov(props.fov.x)} \u00D7 ${formatFov(props.fov.y)}"
          )
        ),
        <.div(
          ExploreStyles.AladinGuideStar,
          Popup(
            content = "Loading catalog stars..",
            position = PopupPosition.TopCenter,
            trigger = Icons.CircleSmall.beat()
          ).when(props.loadingGSCandidates)
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
