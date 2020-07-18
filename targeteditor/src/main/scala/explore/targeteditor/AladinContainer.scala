// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import crystal.react.implicits._
import explore.View
import explore.model.SiderealTarget
import explore.model.TargetVisualOptions
import explore.model.enum.Display
import explore.model.reusability._
import gsp.math.Angle
import gsp.math.Coordinates
import gsp.math.Declination
import gsp.math.HourAngle
import gsp.math.ProperMotion
import gsp.math.RightAscension
import gsp.math.geom.jts.interpreter._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Lens
import org.scalajs.dom.document
import org.scalajs.dom.ext._
import org.scalajs.dom.raw.Element
import react.aladin._
import react.common._

final case class AladinContainer(
  s:       Size,
  target:  View[SiderealTarget],
  options: TargetVisualOptions
) extends ReactProps[AladinContainer](AladinContainer.component) {
  val aladinCoords: Coordinates = target.get.track.baseCoordinates
  val aladinCoordsStr: String   = Coordinates.fromHmsDms.reverseGet(aladinCoords)
}

object AladinContainer {
  type Props = AladinContainer

  protected implicit val propsReuse: Reusability[Props] = Reusability.derive

  val AladinComp = Aladin.component

  class Backend($ : BackendScope[Props, Unit]) {
    // Create a mutable reference
    private val aladinRef = Ref.toScalaComponent(AladinComp)

    private val raLens: Lens[SiderealTarget, RightAscension] =
      SiderealTarget.track ^|-> ProperMotion.baseCoordinates ^|-> Coordinates.rightAscension

    private val decLens: Lens[SiderealTarget, Declination] =
      SiderealTarget.track ^|-> ProperMotion.baseCoordinates ^|-> Coordinates.declination

    def setRa(ra: RightAscension): Callback =
      $.props >>= (_.target.zoom(raLens).set(ra).runInCB)

    def setDec(dec: Declination): Callback =
      $.props >>= (_.target.zoom(decLens).set(dec).runInCB)

    val gotoRaDec = (coords: Coordinates) =>
      aladinRef.get
        .flatMapCB(
          _.backend
            .gotoRaDec(coords.ra.toAngle.toDoubleDegrees, coords.dec.toAngle.toDoubleDegrees)
        )
        .toCallback

    def searchAndGo(modify: ((String, RightAscension, Declination)) => Callback)(search: String) =
      Callback.log(search) *>
        aladinRef.get
          .flatMapCB(
            _.backend
              .gotoObject(
                search,
                (a, b) => {
                  val ra  = RightAscension.fromHourAngle.get(
                    HourAngle.angle.reverseGet(Angle.fromDoubleDegrees(a.toDouble))
                  )
                  val dec =
                    Declination.fromAngle
                      .getOption(Angle.fromDoubleDegrees(b.toDouble))
                      .getOrElse(Declination.Zero)
                  setRa(ra) *> setDec(dec) *> modify((search, ra, dec))
                },
                Callback.log("error")
              )
          )
          .toCallback

    def toggleVisibility(g: Element, selector: String, option: Display): Unit =
      g.querySelectorAll(selector).foreach {
        case e: Element =>
          option.fold(e.classList.remove("visualization-display"),
                      e.classList.add("visualization-display")
          )
      }

    def renderVisualization(
      div:        Element,
      size:       => Size,
      pixelScale: => PixelScale
    ): Callback =
      $.props |> { (p: Props) =>
        val options  = p.options
        // Delete any viz previously rendered
        val previous = Option(div.querySelector(".aladin-visualization"))
        previous.foreach(div.removeChild)
        val g        = document.createElement("div")
        g.classList.add("aladin-visualization")
        visualization.geometryForAladin(GmosGeometry.shapes(options.posAngle),
                                        g,
                                        size,
                                        pixelScale,
                                        GmosGeometry.ScaleFactor
        )
        // Switch the visibility
        toggleVisibility(g, "#science-ccd polygon", options.fov)
        toggleVisibility(g, "#science-ccd-offset polygon", options.offsets)
        toggleVisibility(g, "#patrol-field", options.guiding)
        toggleVisibility(g, "#probe", options.probe)
        div.appendChild(g)
        ()
      }

    def includeSvg(v: JsAladin): Unit = {
      val size = Size(v.getParentDiv().clientHeight, v.getParentDiv().clientWidth)
      val div  = v.getParentDiv()
      v.onFullScreenToggle(recalculateView)
      v.onZoom(renderVisualization(div, size, v.pixelScale))
      ()
    }

    def updateVisualization(v: JsAladin): Callback = {
      val size = Size(v.getParentDiv().clientHeight, v.getParentDiv().clientWidth)
      val div  = v.getParentDiv()
      renderVisualization(div, size, v.pixelScale)
    }

    def render(props: Props) =
      // We want the aladin component inside SizeMe to re-render on resize
      <.div(
        ^.width := 100.pct,
        ^.height := 100.pct,
        AladinComp.withRef(aladinRef) {
          Aladin(showReticle = true,
                 target = props.aladinCoordsStr,
                 fov = 0.25,
                 showGotoControl = false,
                 customize = includeSvg _
          )
        }
      )

    def recalculateView =
      aladinRef.get.flatMapCB(r => r.backend.runOnAladinCB(updateVisualization))
  }

  val component =
    ScalaComponent
      .builder[Props]
      .renderBackend[Backend]
      .componentDidUpdate(_.backend.recalculateView)
      .configure(Reusability.shouldComponentUpdate)
      .build

}
