// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.implicits._
import crystal.react.implicits._
import explore.View
import explore.model.SiderealTarget
import explore.model.TargetVisualOptions
import explore.model.enum.Display
import explore.model.reusability._
import gpp.svgdotjs.svgdotjsSvgJs.mod.Svg
import gpp.ui.reusability._
import gsp.math.Angle
import gsp.math.Coordinates
import gsp.math.Declination
import gsp.math.HourAngle
import gsp.math.ProperMotion
import gsp.math.RightAscension
import gsp.math.geom.jts.interpreter._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Lens
import monocle.macros.Lenses
import org.scalajs.dom.document
import org.scalajs.dom.ext._
import org.scalajs.dom.raw.Element
import react.aladin._
import react.common._
import explore.components.ui.GPPStyles
import react.semanticui.elements.divider.Divider

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
  final case class State(svg: Option[Svg], fov: Fov)

  object State {
    val Zero: State = State(None, Fov(Angle.Angle0, Angle.Angle0))
  }

  protected implicit val propsReuse: Reusability[Props] = Reusability.derive
  protected implicit val stateReuse: Reusability[State] = Reusability.by(_.fov.x)

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
          .flatTap(svg => $.setStateL(State.svg)(svg.some) *> $.setStateL(State.fov)(v.fov))
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
        .flatMap(c => aladinRef.get.flatMapCB(_.backend.world2pix(c))) // calculate the offset
        .zip($.props.map(_.options).toCBO)
        .map {
          case (offsets: (Double, Double), options: TargetVisualOptions) =>
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
                                            offsets
            )
            // Switch the visibility
            toggleVisibility(g, "#science-ccd polygon", options.fov)
            toggleVisibility(g, "#science-ccd-offset polygon", options.offsets)
            toggleVisibility(g, "#patrol-field", options.guiding)
            toggleVisibility(g, "#probe", options.probe)
            div.appendChild(g)
            ()
        }

    def includeSvg(v: JsAladin): Callback =
      v.onFullScreenToggle(recalculateView) *> // re render on screen toggle
        v.onZoom(onZoom(v)) *>                 // re render on zoom
        v.onPositionChanged(onPositionChanged(v)) *>
        v.onMouseMove(s => Callback.log(s"$s"))

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
        .flatMap {
          case (p, s) =>
            val size     = Size(v.getParentDiv().clientHeight, v.getParentDiv().clientWidth)
            val div      = v.getParentDiv()
            // Update the existing visualization in place
            val previous = Option(div.querySelector(".aladin-visualization"))
            (s.svg, previous).mapN {
              case (svg, previous) =>
                aladinRef.get
                  .flatMapCB(
                    _.backend.world2pix(Coordinates(p.aladinCoords.ra, p.aladinCoords.dec))
                  )
                  .flatMapCB { off =>
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
                  }
                  .toCallback
            }.getOrEmpty
        }
        .void

    def formatAngle(angle: Angle): String = {
      val dms     = Angle.DMS(angle)
      val degrees = if (dms.degrees > 180) s"-${360 - dms.degrees}" else dms.degrees.toString
      val minutes = "%02d".format(dms.arcminutes)
      s"$degrees°$minutes"
    }

    def render(props: Props, state: State) =
      // We want the aladin component inside SizeMe to re-render on resize
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
        <.div(GPPStyles.AladinContainerStatus,
              s"Fov: ${formatAngle(state.fov.x)}",
              Divider(vertical = true)
        )
      )

    def recalculateView =
      aladinRef.get.flatMapCB { r =>
        r.backend.runOnAladinCB { v =>
          updateSvgState(v).flatMap { s =>
            r.backend.recalculateView *> r.backend.runOnAladinCB(updateVisualization(s))
          }
        }
      }
  }

  val component =
    ScalaComponent
      .builder[Props]
      .initialState(State.Zero)
      .renderBackend[Backend]
      .componentDidMount(_.backend.initialSvgState.void)
      .componentDidUpdate(_.backend.recalculateView)
      .configure(Reusability.shouldComponentUpdate)
      .build

}
