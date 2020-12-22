// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import scala.math.rint

import explore.Icons
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.HourAngle.HMS
import lucuma.core.math._
import lucuma.ui.reusability._
import react.aladin.Fov
import react.aladin.reusability._
import react.common.ReactProps
import react.semanticui.elements.label._
import react.semanticui.sizes.Small

final case class AladinToolbar(
  fov:     Fov,
  current: Coordinates
) extends ReactProps[AladinToolbar](AladinToolbar.component)

object AladinToolbar {
  type Props = AladinToolbar

  implicit val propsReuse: Reusability[Props] = Reusability.derive

  // TODO: We may want to move these to gsp-math
  def formatHMS(hms: HMS): String =
    f"${hms.hours}%02d:${hms.minutes}%02d:${hms.seconds}%02d"

  val fromStringDMS: Angle => String =
    dms => {
      val r = Angle.dms.get(dms)
      f"${r.degrees}%02d:${r.arcminutes}%02d:${r.arcseconds}%02d"
    }

  val fromStringSignedDMS: Angle => String =
    a =>
      if (Angle.signedMicroarcseconds.get(a) < 0) "-" + fromStringDMS(-a)
      else "+" + fromStringDMS(a)

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
    ScalaComponent
      .builder[Props]
      .render_P((props: Props) =>
        React.Fragment(
          Label(
            icon = Icons.Expand.fitted(true).clazz(ExploreStyles.Accented),
            clazz = ExploreStyles.AladinFOV,
            size = Small,
            detail = LabelDetail(clazz = ExploreStyles.AladinDetailText,
                                 content =
                                   s"${formatFov(props.fov.x)} \u00D7 ${formatFov(props.fov.y)}"
            )
          ),
          Label(
            icon = Icons.MousePointer.fitted(true).clazz(ExploreStyles.Accented),
            clazz = ExploreStyles.AladinCurrentCoords,
            size = Small,
            detail = LabelDetail(clazz = ExploreStyles.AladinDetailText,
                                 content = formatCoordinates(props.current)
            )
          )
        )
      )
      .configure(Reusability.shouldComponentUpdate)
      .build
}
