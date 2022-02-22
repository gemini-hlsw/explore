// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.all._
import crystal.react.View
import crystal.react.implicits._
import crystal.react.reuse._
import explore.components.ui.ExploreStyles
import explore.model.TargetVisualOptions
import explore.model.enum.Visible
import explore.model.reusability._
import japgolly.scalajs.react.ReactMonocle._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.geom.jts.interpreter._
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.svgdotjs.Svg
import lucuma.ui.reusability._
import monocle.Focus
import org.scalajs.dom.Element
import org.scalajs.dom.document
import react.aladin._
import react.common._

import scala.annotation.nowarn
import scala.concurrent.duration._

final case class AladinContainer( // TODO Use target's FOV if not set in user prefs.
  target:                 View[Coordinates],
  options:                TargetVisualOptions,
  updateMouseCoordinates: Coordinates ==> Callback,
  updateFov:              Fov ==> Callback
) extends ReactProps[AladinContainer](AladinContainer.component) {
  val aladinCoords: Coordinates = target.get
  val aladinCoordsStr: String   = Coordinates.fromHmsDms.reverseGet(aladinCoords)
}

object AladinContainer {
  type Props = AladinContainer

  /**
   * On the state we keep the svg to avoid recalculations during panning
   */
  final case class State(svg: Option[Svg])

  object State {
    val svg         = Focus[State](_.svg)
    val Zero: State = State(None)
  }

  protected implicit val propsReuse: Reusability[Props] = Reusability.by(x => (x.target, x.options))
  // Reuse always becasue the svg is renderd only once
  protected implicit val stateReuse: Reusability[State] = Reusability.always

  val AladinComp = Aladin.component

  class Backend($ : BackendScope[Props, State]) {
    // Create a mutable reference
    private val aladinRef = Ref.toScalaComponent(AladinComp)

    def setRa(ra: RightAscension): Callback =
      $.props.flatMap(_.target.zoom(Coordinates.rightAscension).set(ra))

    def setDec(dec: Declination): Callback =
      $.props.flatMap(_.target.zoom(Coordinates.declination).set(dec))

    val gotoRaDec = (coords: Coordinates) =>
      aladinRef.get.asCBO
        .flatMapCB(
          _.backend
            .gotoRaDec(coords.ra.toAngle.toDoubleDegrees, coords.dec.toAngle.toDoubleDegrees)
        )
        .toCallback

    def searchAndGo(
      modify: ((String, RightAscension, Declination)) => Callback
    )(search: String) =
      aladinRef.get.asCBO
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

    def toggleVisibility(g: Element, selector: String, option: Visible): Unit =
      g.querySelectorAll(selector).foreach {
        case e: Element =>
          option.fold(
            e.classList.remove("visualization-display"),
            e.classList.add("visualization-display")
          )
        case _          => ()
      }

    /**
     * Recalculate the svg, we keep it on the state for better performance
     *
     * @return
     */
    def initialSvgState: Callback =
      aladinRef.get.asCBO
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
          .flatTap(svg => $.setStateL(State.svg)(svg.some) *> $.props.flatMap(_.updateFov(v.fov)))
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
        .flatMap(c => aladinRef.get.asCBO.flatMapCB(_.backend.world2pix(c)))
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
        v.onMouseMove(s =>
          $.props.flatMap(
            _.updateMouseCoordinates(Coordinates(s.ra, s.dec))
              .debounce(100.millis)
              .void
          )
        )

    def updateVisualization(s: Svg)(v: JsAladin): Callback = {
      val size = Size(v.getParentDiv().clientHeight, v.getParentDiv().clientWidth)
      val div  = v.getParentDiv()
      renderVisualization(s, div, size, v.pixelScale)
    }

    def updateVisualization(v: JsAladin): Callback =
      $.state.flatMap(s => s.svg.map(updateVisualization(_)(v)).getOrEmpty)

    def onZoom(v: JsAladin): Callback =
      updateSvgState(v).flatMap { s =>
        aladinRef.get.asCBO.flatMapCB(r =>
          r.backend.recalculateView *>
            r.backend.runOnAladinCB(updateVisualization(s))
        )
      }

    /**
     * Called when the position changes, i.e. aladin pans. We want to offset the visualization to
     * keep the internal target correct
     */
    @nowarn
    def onPositionChanged(v: JsAladin)(u: PositionChanged): Callback =
      $.props
        .zip($.state)
        .flatMap { case (p, s) =>
          val size     = Size(v.getParentDiv().clientHeight, v.getParentDiv().clientWidth)
          val div      = v.getParentDiv()
          // Update the existing visualization in place
          val previous = Option(div.querySelector(".aladin-visualization"))
          (s.svg, previous).mapN { case (svg, previous) =>
            aladinRef.get.asCBO
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
        aladinRef.get.asCBO.flatMapCB(
          _.backend.gotoRaDec(p.aladinCoords.ra.toAngle.toDoubleDegrees,
                              p.aladinCoords.dec.toAngle.toSignedDoubleDegrees
          )
        )
      )

    def render(props: Props) =
      <.div(
        ExploreStyles.AladinContainerBody
        // AladinComp.withRef(aladinRef) {
        // Aladin(
        //   ExploreStyles.TargetAladin,
        //   showReticle = false,
        //   showLayersControl = false,
        //   target = props.aladinCoordsStr,
        //   fov = props.options.fovAngle.toDoubleDegrees,
        //   showGotoControl = false,
        //   customize = includeSvg _
        // )
        // }
      )

    def recalculateView =
      aladinRef.get.asCBO.flatMapCB { r =>
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
      .componentDidUpdate(_.backend.recalculateView.toCallback)
      .configure(Reusability.shouldComponentUpdate)
      .build
}
