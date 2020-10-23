// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import scala.math.rint

import explore.components.ui.ExploreStyles
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.Angle.DMS
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

  def formatDMS(dms: DMS): String = {
    val prefix = if (dms.toAngle.toMicroarcseconds < 0) "-" else "+"
    f"$prefix${dms.degrees}%02d:${dms.arcminutes}%02d:${dms.arcseconds}%02d"
  }

  def formatCoordinates(coords: Coordinates): String = {
    val ra  = HMS(coords.ra.toHourAngle)
    val dec = DMS(coords.dec.toAngle)
    s"${formatHMS(ra)} ${formatDMS(dec)}"
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
    else if (arcminutes >= 10)
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
            content = "Fov:",
            clazz = ExploreStyles.AladinFOV,
            size = Small,
            detail =
              LabelDetail(clazz = ExploreStyles.AladinDetailText, content = formatFov(props.fov.x))
          ),
          Label(
            content = "Cur:",
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
