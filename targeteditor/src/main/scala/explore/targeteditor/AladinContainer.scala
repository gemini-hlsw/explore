// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import scala.math.rint

import cats.syntax.all._
import crystal.react.implicits._
import explore.Icons
import explore.View
import explore.components.ui.GPPStyles
import explore.model.SiderealTarget
import explore.model.TargetVisualOptions
import explore.model.enum.Display
import explore.model.reusability._
import gpp.svgdotjs.svgdotjsSvgJs.mod.Svg
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.geom.jts.interpreter._
import lucuma.core.math.Angle
import lucuma.core.math.Angle.DMS
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.HourAngle.HMS
import lucuma.core.math.ProperMotion
import lucuma.core.math.RightAscension
import lucuma.ui.reusability._
import monocle.Lens
import monocle.macros.Lenses
import org.scalajs.dom.document
import org.scalajs.dom.ext._
import org.scalajs.dom.raw.Element
import react.aladin._
import react.common._
import react.semanticui.elements.button.Button
import react.semanticui.elements.label.Label
import react.semanticui.elements.label.LabelDetail
import react.semanticui.modules.popup.Popup
import react.semanticui.sizes._

@Lenses
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

  /**
   * On the state we keep the svg to avoid recalculations during panning
   */
  @Lenses
  final case class State(svg: Option[Svg], fov: Fov, current: Coordinates)

  object State {
    val Zero: State = State(None, Fov(Angle.Angle0, Angle.Angle0), Coordinates.Zero)
  }

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

  protected implicit val propsReuse: Reusability[Props] = Reusability.derive
  protected implicit val stateReuse: Reusability[State] =
    Reusability.by(s => (formatFov(s.fov.x), formatCoordinates(s.current)))

  val AladinComp = Aladin.component

  class Backend($ : BackendScope[Props, State]) {
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
                  val ra  = RightAscension.fromDoubleDegrees(a.toDouble)
                  val dec = Declination.fromDoubleDegrees(b.toDouble).getOrElse(Declination.Zero)
                  setRa(ra) *> setDec(dec) *> modify((search, ra, dec))
                },
                Callback.log("error")
              )
          )
          .toCallback

    def toggleVisibility(g: Element, selector: String, option: Display): Unit =
      g.querySelectorAll(selector).foreach { case e: Element =>
        option.fold(e.classList.remove("visualization-display"),
                    e.classList.add("visualization-display")
        )
      }

    /**
     * Recalculate the svg, we keep it on the state for better performance
     *
     * @return
     */
    def initialSvgState: Callback =
      aladinRef.get
        .flatMapCB(_.backend.runOnAladinCB(updateSvgState))
        .void

    /**
     * Recalculate svg and store it on state
     *
     * @param pixelScale
     * @return
     */
    def updateSvgState(v: JsAladin): CallbackTo[Svg] =
      $.props.flatMap { p =>
        CallbackTo
          .pure(
            visualization
              .shapesToSvg(GmosGeometry.shapes(p.options.posAngle),
                           GmosGeometry.pp,
                           v.pixelScale,
                           GmosGeometry.ScaleFactor
              )
          )
          .flatTap(svg => $.modState((s: State) => s.copy(svg = svg.some, fov = v.fov)))
      }

    def renderVisualization(
      svgBase:    Svg,
      div:        Element,
      size:       => Size,
      pixelScale: => PixelScale
    ): Callback =
      $.props
        .map(_.aladinCoords)
        .toCBO
        // calculate the offset
        .flatMap(c => aladinRef.get.flatMapCB(_.backend.world2pix(c)))
        .zip($.props.map(_.options).toCBO)
        .map {
          case (Some((x, y)), options: TargetVisualOptions) =>
            // Delete any viz previously rendered
            val previous = Option(div.querySelector(".aladin-visualization"))
            previous.foreach(div.removeChild)
            val g        = document.createElement("div")
            g.classList.add("aladin-visualization")
            visualization.geometryForAladin(svgBase,
                                            g,
                                            size,
                                            pixelScale,
                                            GmosGeometry.ScaleFactor,
                                            (x, y)
            )
            // Switch the visibility
            toggleVisibility(g, "#science-ccd polygon", options.fov)
            toggleVisibility(g, "#science-ccd-offset polygon", options.offsets)
            toggleVisibility(g, "#patrol-field", options.guiding)
            toggleVisibility(g, "#probe", options.probe)
            div.appendChild(g)
            ()
          case _                                            =>
        }

    def includeSvg(v: JsAladin): Callback =
      v.onFullScreenToggle(recalculateView) *> // re render on screen toggle
        v.onZoom(onZoom(v)) *>                 // re render on zoom
        v.onPositionChanged(onPositionChanged(v)) *>
        v.onMouseMove(s => $.setStateL(State.current)(Coordinates(s.ra, s.dec)))

    def updateVisualization(s: Svg)(v: JsAladin): Callback = {
      val size = Size(v.getParentDiv().clientHeight, v.getParentDiv().clientWidth)
      val div  = v.getParentDiv()
      renderVisualization(s, div, size, v.pixelScale)
    }

    def updateVisualization(v: JsAladin): Callback =
      $.state.flatMap(s => s.svg.map(updateVisualization(_)(v)).getOrEmpty)

    def onZoom(v: JsAladin): Callback =
      updateSvgState(v).flatMap { s =>
        aladinRef.get.flatMapCB(r =>
          r.backend.recalculateView *>
            r.backend.runOnAladinCB(updateVisualization(s))
        )
      }

    /**
     * Called when the position changes, i.e. aladin pans. We want to offset the visualization to
     * keep the internal target correct
     */
    def onPositionChanged(v: JsAladin)(s: PositionChanged): Callback =
      $.props
        .zip($.state)
        .flatMap { case (p, s) =>
          val size     = Size(v.getParentDiv().clientHeight, v.getParentDiv().clientWidth)
          val div      = v.getParentDiv()
          // Update the existing visualization in place
          val previous = Option(div.querySelector(".aladin-visualization"))
          (s.svg, previous).mapN { case (svg, previous) =>
            aladinRef.get
              .flatMapCB(
                _.backend.world2pix(Coordinates(p.aladinCoords.ra, p.aladinCoords.dec))
              )
              .flatMapCB {
                case Some(off) =>
                  Callback {
                    // Offset the visualization
                    visualization
                      .updatePosition(svg,
                                      previous,
                                      size,
                                      v.pixelScale,
                                      GmosGeometry.ScaleFactor,
                                      off
                      )
                  }
                case _         => Callback.empty
              }
              .toCallback
          }.getOrEmpty
        }
        .void

    def centerOnTarget: Callback =
      $.props.flatMap(p =>
        aladinRef.get.flatMapCB(
          _.backend.gotoRaDec(p.aladinCoords.ra.toAngle.toDoubleDegrees,
                              p.aladinCoords.dec.toAngle.toSignedDoubleDegrees
          )
        )
      )

    def render(props: Props, state: State) =
      <.div(
        GPPStyles.AladinContainerColumn,
        <.div(
          GPPStyles.AladinContainerBody,
          AladinComp.withRef(aladinRef) {
            Aladin(showReticle = false,
                   showLayersControl = false,
                   target = props.aladinCoordsStr,
                   fov = 0.25,
                   showGotoControl = false,
                   customize = includeSvg _
            )
          }
        ),
        Label(content = "Fov:",
              clazz = GPPStyles.AladinFOV,
              size = Small,
              detail =
                LabelDetail(clazz = GPPStyles.AladinDetailText, content = formatFov(state.fov.x))
        ),
        Label(
          content = "Cur:",
          clazz = GPPStyles.AladinCurrentCoords,
          size = Small,
          detail = LabelDetail(clazz = GPPStyles.AladinDetailText,
                               content = formatCoordinates(state.current)
          )
        ),
        <.div(
          GPPStyles.AladinCenterButton,
          Popup(content = "Center on target",
                trigger = Button(size = Mini, icon = true, onClick = centerOnTarget)(Icons.Bullseye)
          )
        )
      )

    def recalculateView =
      aladinRef.get.flatMapCB { r =>
        r.backend.runOnAladinCB { v =>
          updateSvgState(v).flatMap(s =>
            r.backend.recalculateView *> r.backend.runOnAladinCB(updateVisualization(s))
          )
        }
      }
  }

  val component =
    ScalaComponent
      .builder[Props]
      .initialState(State.Zero)
      .renderBackend[Backend]
      .componentDidMount($ =>
        $.backend.initialSvgState *> $.setStateL(State.current)($.props.aladinCoords)
      )
      .componentDidUpdate(_.backend.recalculateView)
      .configure(Reusability.shouldComponentUpdate)
      .build

}
