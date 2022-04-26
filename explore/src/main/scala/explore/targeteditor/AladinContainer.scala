// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import crystal.react.ReuseView
import crystal.react.hooks._
import crystal.react.reuse._
import explore.components.ui.ExploreStyles
import explore.model.ScienceConfiguration
import explore.model.enum.Visible
import explore.model.reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.geom.jts.interpreter._
import lucuma.core.math.Coordinates
import lucuma.svgdotjs.Svg
import lucuma.ui.reusability._
import org.scalajs.dom.Element
import org.scalajs.dom.document
import react.aladin._
import react.aladin.reusability._
import react.common._
import react.resizeDetector.hooks._

import scala.concurrent.duration._

final case class AladinContainer(
  target:                 ReuseView[Coordinates],
  configuration:          Option[ScienceConfiguration],
  fov:                    Fov,
  updateMouseCoordinates: Coordinates ==> Callback,
  updateFov:              Fov ==> Callback, // TODO Move the functionality of saving the FOV in ALadincell here
  centerOnTarget:         ReuseView[Boolean]
) extends ReactFnProps[AladinContainer](AladinContainer.component) {
  val aladinCoords: Coordinates = target.get
  val aladinCoordsStr: String   = Coordinates.fromHmsDms.reverseGet(aladinCoords)
}

object AladinContainer {
  type Props       = AladinContainer
  type World2PixFn = Coordinates => Option[(Double, Double)]
  val DefaultWorld2PixFn: World2PixFn = (_: Coordinates) => None

  // This is used for screen coordinates, thus it doesn't need a lot of precission
  private implicit val doubleReuse                      = Reusability.double(1.0)
  private implicit val fovReuse                         = exactFovReuse
  protected implicit val propsReuse: Reusability[Props] =
    Reusability.by((x: Props) => (x.target, x.configuration, x.fov))

  val AladinComp = Aladin.component

  def updateVisualization(svg: Svg, off: (Double, Double))(v: JsAladin): Callback = {
    val size = Size(v.getParentDiv().clientHeight.toDouble, v.getParentDiv().clientWidth.toDouble)
    val div  = v.getParentDiv()
    renderVisualization(svg, off, div, size, v.pixelScale)
  }

  def renderVisualization(
    svg:        Svg,
    offset:     (Double, Double),
    div:        Element,
    size:       Size,
    pixelScale: PixelScale
  ): Callback =
    Callback {
      val (x, y) = offset
      // Delete any viz previously rendered
      val g      = Option(div.querySelector(".aladin-visualization"))
        .map { g =>
          g.childNodes.toList.foreach(g.removeChild)
          g
        }
        .getOrElse {
          val g = document.createElement("div")
          g.classList.add("aladin-visualization")
          // Include the svg on the dom
          div.appendChild(g)
          g
        }
      // Render the svg
      visualization.geometryForAladin(svg, g, size, pixelScale, GmosGeometry.ScaleFactor, (x, y))
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

  val component =
    ScalaFnComponent
      .withHooks[Props]
      // View coordinates (in case the user pans)
      .useStateBy(_.aladinCoords)
      // Memoized svg
      .useMemoBy((p, _) => (p.configuration, p.fov)) { case (p, _) =>
        _ =>
          visualization
            .shapesToSvg(GmosGeometry.shapes(GmosGeometry.posAngle, p.configuration),
                         GmosGeometry.pp,
                         GmosGeometry.ScaleFactor
            )
      }
      // Ref to the aladin component
      .useRefToScalaComponent(AladinComp)
      // If needed center on target
      .useEffectWithDepsBy((p, _, _, _) => (p.aladinCoords, p.centerOnTarget.get))(
        (_, _, _, aladinRef) => { case (coords, center) =>
          aladinRef.get.asCBO
            .flatMapCB(
              _.backend.gotoRaDec(coords.ra.toAngle.toDoubleDegrees,
                                  coords.dec.toAngle.toSignedDoubleDegrees
              )
            )
            .when(center)
        }
      )
      // Function to calculate coordinates
      .useSerialState(DefaultWorld2PixFn)
      // resize detector
      .useResizeDetector()
      // Update the world2pix function
      .useEffectWithDepsBy { (p, currentPos, _, aladinRef, _, resize) =>
        (resize, p.fov, currentPos, aladinRef)
      } { (_, _, _, aladinRef, w, _) => _ =>
        aladinRef.get.asCBO.flatMapCB(_.backend.world2pixFn.flatMap(w.setState))
      }
      // Render the visualization, only if current pos, fov or size changes
      .useEffectWithDepsBy((p, currentPos, _, _, world2pix, resize) =>
        (p.fov, p.configuration, currentPos, world2pix.value(p.aladinCoords), resize)
      ) { (_, _, svg, aladinRef, _, _) =>
        { case (_, _, _, off, _) =>
          off
            .map(off =>
              aladinRef.get.asCBO
                .flatMapCB(r =>
                  r.backend
                    .runOnAladinCB(updateVisualization(svg, off)) *> r.backend.fixLayoutDimensions
                )
                .toCallback
            )
            .getOrEmpty
        }
      }
      .renderWithReuse { (props, currentPos, _, aladinRef, _, resize) =>
        /**
         * Called when the position changes, i.e. aladin pans. We want to offset the visualization
         * to keep the internal target correct
         */
        def onPositionChanged(u: PositionChanged): Callback =
          currentPos.setState(Coordinates(u.ra, u.dec)) *> props.centerOnTarget.set(false)

        def onZoom = (v: Fov) => props.updateFov(v)

        def includeSvg(v: JsAladin): Callback =
          v.onZoom(onZoom) *> // re render on zoom
            v.onPositionChanged(onPositionChanged) *>
            v.onMouseMove(s =>
              props
                .updateMouseCoordinates(Coordinates(s.ra, s.dec))
                .rateLimit(200.millis, 1)
                .void
            )

        <.div(
          ExploreStyles.AladinContainerBody,
          // This is a bit tricky. Sometimes the height can be 0 or a very low number.
          // This happens during a second render. If we let the height to be zero, aladin
          // will take it as 1. This height ends up being a denominator, which, if low,
          // will make aladin request a large amount of tiles and end up freeze explore.
          if (resize.height.exists(_ >= 100))
            AladinComp.withRef(aladinRef) {
              Aladin(
                ExploreStyles.TargetAladin,
                showReticle = false,
                showLayersControl = false,
                target = props.aladinCoordsStr,
                fov = props.fov.x,
                showGotoControl = false,
                customize = includeSvg _
              )
            }
          else EmptyVdom
        )
          .withRef(resize.ref)
      }
}
