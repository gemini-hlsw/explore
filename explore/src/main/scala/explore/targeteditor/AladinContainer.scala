// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.all._
import crystal.react.ReuseView
import crystal.react.hooks._
import crystal.react.reuse._
import explore.components.ui.ExploreStyles
import explore.model.ScienceConfiguration
import explore.model.TargetVisualOptions
import explore.model.enum.Visible
import explore.model.reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.geom.jts.interpreter._
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.math.Offset
import lucuma.svgdotjs.Svg
import lucuma.ui.reusability._
import org.scalajs.dom.Element
import org.scalajs.dom.document
import react.aladin._
import react.common._
import react.resizeDetector.hooks._

import scala.concurrent.duration._

final case class AladinContainer(
  target:                 ReuseView[Coordinates],
  configuration:          Option[ScienceConfiguration],
  options:                TargetVisualOptions,
  updateMouseCoordinates: Coordinates ==> Callback,
  updateFov:              Fov ==> Callback, // TODO Move the functionality of saving the FOV in ALadincell here
  updateViewOffset:       Offset ==> Callback,
  centerOnTarget:         ReuseView[Boolean]
) extends ReactFnProps[AladinContainer](AladinContainer.component) {
  val baseCoordinates: Coordinates     = target.get
  val baseCoordinatesForAladin: String = Coordinates.fromHmsDms.reverseGet(baseCoordinates)
}

object AladinContainer {

  implicit class CoordinatesOps(val c: Coordinates) extends AnyVal {
    def offsetBy(posAngle: Angle, o: Offset): Option[Coordinates] = {
      val paCos  = posAngle.cos
      val paSin  = posAngle.sin
      val pDeg   = o.p.toAngle.toSignedDoubleDegrees
      val qDeg   = o.q.toAngle.toSignedDoubleDegrees
      val dRa    = pDeg * paCos + qDeg * paSin
      val dDec   = -pDeg * paSin + qDeg * paCos
      val decCos = c.dec.toAngle.cos

      Declination
        .fromDoubleDegrees(c.dec.toAngle.toSignedDoubleDegrees + dDec)
        .filter(_ => decCos != 0)
        .map { d =>
          Coordinates(RightAscension.fromDoubleDegrees(c.ra.toAngle.toDoubleDegrees + dRa / decCos),
                      d
          )
        }
    }
  }

  type Props       = AladinContainer
  type World2PixFn = Coordinates => Option[(Double, Double)]
  val DefaultWorld2PixFn: World2PixFn = (_: Coordinates) => None

  // This is used for screen coordinates, thus it doesn't need a lot of precission
  private implicit val doubleReuse                      = Reusability.double(1.0)
  protected implicit val propsReuse: Reusability[Props] = Reusability.derive

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
      .useStateBy { p =>
        p.baseCoordinates.offsetBy(Angle.Angle0, p.options.viewOffset)
      }
      // Memoized svg
      .useMemoBy((p, _) => (p.configuration, p.options)) { case (p, _) =>
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
      .useEffectWithDepsBy((p, _, _, _) => (p.baseCoordinates, p.centerOnTarget.get))(
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
        (resize, p.options, currentPos, aladinRef)
      } { (_, _, _, aladinRef, w, _) => _ =>
        aladinRef.get.asCBO.flatMapCB(_.backend.world2pixFn.flatMap(w.setState))
      }
      // Render the visualization, only if current pos, fov or size changes
      .useEffectWithDepsBy((p, currentPos, _, _, world2pix, resize) =>
        (p.options.fovAngle,
         p.configuration,
         currentPos,
         world2pix.value(p.baseCoordinates),
         resize
        )
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
        def onPositionChanged(u: PositionChanged): Callback = {
          val viewCoords = Coordinates(u.ra, u.dec)
          val viewOffset = props.baseCoordinates.diff(viewCoords).offset
          currentPos.setState(Some(viewCoords)) *>
            props.updateViewOffset(viewOffset) *>
            props.centerOnTarget.set(false)
        }

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

        val baseCoordinatesForAladin: String =
          currentPos.value
            .map(Coordinates.fromHmsDms.reverseGet)
            .getOrElse(props.baseCoordinatesForAladin)

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
                target = baseCoordinatesForAladin,
                fov = props.options.fovAngle,
                showGotoControl = false,
                customize = includeSvg _
              )
            }
          else EmptyVdom
        )
          .withRef(resize.ref)
      }
}
