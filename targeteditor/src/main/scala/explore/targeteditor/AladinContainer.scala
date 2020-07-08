package explore.targeteditor

import explore.model.TargetVisualOptions
import explore.model.enum.Display
import explore.model.reusability._
import gsp.math.Coordinates
import gsp.math.geom.jts.interpreter._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.document
import org.scalajs.dom.ext._
import org.scalajs.dom.raw.Element
import react.aladin._
import react.common._

final case class AladinContainer(s: Size, coordinates: Coordinates, options: TargetVisualOptions)
    extends ReactProps[AladinContainer](AladinContainer.component) {
  val aladinCoordsStr: String = Coordinates.fromHmsDms.reverseGet(coordinates)
}

object AladinContainer {
  type Props = AladinContainer

  protected implicit val propsReuse: Reusability[Props] = Reusability.derive

  val AladinComp = Aladin.component

  class Backend($ : BackendScope[Props, Unit]) {
    // Create a mutable reference
    private val ref = Ref.toScalaComponent(AladinComp)

    def toggleVisibility(g: Element, selector: String, option: Display): Unit =
      g.querySelectorAll(selector).foreach {
        case e: Element =>
          option.fold(e.classList.remove("visualization-display"),
                      e.classList.add("visualization-display")
          )
      }

    def renderVisualization(
      div:        Element,
      size:       Size,
      pixelScale: => PixelScale
    ): Callback =
      $.props |> { (p: Props) =>
        val options  = p.options
        println(pixelScale)
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
      v.onZoomCB(renderVisualization(div, size, v.pixelScale))
      ()
    }

    def updateVisualization(v: JsAladin): Callback = {
      println(v.getParentDiv().classList)
      val size = Size(v.getParentDiv().clientHeight, v.getParentDiv().clientWidth)
      println(size.width)
      val div  = v.getParentDiv()
      renderVisualization(div, size, v.pixelScale)
    }

    def render(props: Props) =
      // We want the aladin component inside SizeMe to re-render on resize
      <.div(
        ^.width := 100.pct,
        ^.height := 100.pct,
        AladinComp.withRef(ref) {
          Aladin(showReticle = true,
                 target = props.aladinCoordsStr,
                 fov = 0.25,
                 showGotoControl = false,
                 customize = includeSvg _
          )
        }
      )

    def recalculateView =
      Callback.log("didupdate") *>
        ref.get.flatMapCB(r => r.backend.runOnAladinCB(updateVisualization))
  }

  val component =
    ScalaComponent
      .builder[Props]
      .renderBackend[Backend]
      .componentDidUpdate(_.backend.recalculateView)
      .configure(Reusability.shouldComponentUpdate)
      .build

}
