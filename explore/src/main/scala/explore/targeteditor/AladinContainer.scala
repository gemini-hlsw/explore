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
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.geom.jts.interpreter._
import lucuma.core.math.Coordinates
import lucuma.ui.reusability._
import org.scalajs.dom.Element
import org.scalajs.dom.document
import react.aladin._
import react.aladin.reusability._
import react.common._
import react.resizeDetector.hooks._

import scala.annotation.nowarn
import scala.concurrent.duration._

final case class AladinContainer(
  target:                 View[Coordinates],
  options:                TargetVisualOptions,
  fov:                    View[Fov],
  updateMouseCoordinates: Coordinates ==> Callback,
  updateFov:              Fov ==> Callback, // TODO Move the functionality of saving the FOV in ALadincell here
  centerOnTarget:         View[Boolean]
) extends ReactFnProps[AladinContainer](AladinContainer.component) {
  val aladinCoords: Coordinates = target.get
  val aladinCoordsStr: String   = Coordinates.fromHmsDms.reverseGet(aladinCoords)
}

object AladinContainer {
  type Props = AladinContainer

  protected implicit val propsReuse: Reusability[Props] =
    Reusability.by(x => (x.target, x.options, x.fov))

  val AladinComp = Aladin.component

  val component =
    ScalaFnComponent
      .withHooks[Props]
      // Memoized svg
      .useMemoBy(p => (p.options.posAngle, p.fov))(_ => { case (posAngle, _) =>
        visualization
          .shapesToSvg(GmosGeometry.shapes(posAngle), GmosGeometry.pp, GmosGeometry.ScaleFactor)
      })
      // Ref to the aladin component
      .useRefToScalaComponent(AladinComp)
      // If needed center on target
      .useEffectWithDepsBy((p, _, _) => (p.aladinCoords, p.centerOnTarget.get))(
        (_, _, aladinRef) => { case (coords, center) =>
          aladinRef.get.asCBO
            .flatMapCB(
              _.backend.gotoRaDec(coords.ra.toAngle.toDoubleDegrees,
                                  coords.dec.toAngle.toSignedDoubleDegrees
              )
            )
            .when(center)
        }
      )
      .useResizeDetector()
      .render { (props, svg, aladinRef, resize) =>
        /**
         * Called when the position changes, i.e. aladin pans. We want to offset the visualization
         * to keep the internal target correct
         */
        @nowarn
        def onPositionChanged(v: JsAladin)(u: PositionChanged): Callback = {
          val size     = Size(v.getParentDiv().clientHeight, v.getParentDiv().clientWidth)
          val div      = v.getParentDiv()
          // Update the existing visualization in place
          val previous = Option(div.querySelector(".aladin-visualization"))
          (svg.value.some, previous).mapN { case (svg, previous) =>
            aladinRef.get.asCBO
              .flatMapCB(
                _.backend.world2pix(props.aladinCoords)
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
          }.getOrEmpty *> props.centerOnTarget.set(false) // Reset center or we we cannot pan
        }

        def toggleVisibility(g: Element, selector: String, option: Visible): Unit =
          g.querySelectorAll(selector).foreach {
            case e: Element =>
              option.fold(
                e.classList.remove("visualization-display"),
                e.classList.add("visualization-display")
              )
            case _          => ()
          }

        def renderVisualization(
          div:        Element,
          size:       => Size,
          pixelScale: => PixelScale
        ): Callback =
          // calculate the offset
          aladinRef.get.asCBO
            .flatMapCB(_.backend.world2pix(props.aladinCoords))
            .map {
              case Some((x, y)) =>
                // Delete any viz previously rendered
                val previous = Option(div.querySelector(".aladin-visualization"))
                previous.foreach(div.removeChild)
                val g        = document.createElement("div")
                g.classList.add("aladin-visualization")
                visualization.geometryForAladin(svg.value,
                                                g,
                                                size,
                                                pixelScale,
                                                GmosGeometry.ScaleFactor,
                                                (x, y)
                )
                // Switch the visibility
                toggleVisibility(g, "#science-ccd polygon", props.options.fov)
                toggleVisibility(g, "#science-ccd-offset polygon", props.options.offsets)
                toggleVisibility(g, "#patrol-field", props.options.guiding)
                toggleVisibility(g, "#probe", props.options.probe)
                div.appendChild(g)
                ()
              case _            =>
            }

        def updateVisualization(v: JsAladin): Callback = {
          val size = Size(v.getParentDiv().clientHeight, v.getParentDiv().clientWidth)
          val div  = v.getParentDiv()
          renderVisualization(div, size, v.pixelScale)
        }

        def onZoom = (v: Fov) =>
          props.fov.set(v) *>
            props.updateFov(v) *> recalculateView

        def recalculateView =
          aladinRef.get.asCBO.flatMapCB { r =>
            r.backend.recalculateView *> r.backend.runOnAladinCB(updateVisualization)
          }

        def includeSvg(v: JsAladin): Callback =
          v.onFullScreenToggle(recalculateView) *> // re render on screen toggle
            v.onZoom(onZoom) *>                    // re render on zoom
            v.onPositionChanged(onPositionChanged(v)) *>
            v.onMouseMove(s =>
              props
                .updateMouseCoordinates(Coordinates(s.ra, s.dec))
                .debounce(200.millis)
                .void
            )

        <.div(
          ExploreStyles.AladinContainerBody,
          // This is a bit tricky. Sometimes the height can be 0, this happens if
          // during a second render. If we let the height to be zero, aladin will
          // take it as 1 and miss calculate the amount of tiles needed.
          // This can endup requesting a large amount of tiles and would
          // freeze explore
          if (resize.height.exists(_ > 0))
            AladinComp.withRef(aladinRef) {
              Aladin(
                ExploreStyles.TargetAladin,
                showReticle = false,
                showLayersControl = false,
                target = props.aladinCoordsStr,
                fov = props.fov.get.x,
                showGotoControl = false,
                customize = includeSvg _
              )
            }
          else EmptyVdom
        )
          .withRef(resize.ref)
      }
}
