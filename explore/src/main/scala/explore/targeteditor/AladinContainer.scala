// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.all._
import crystal.react.ReuseView
import crystal.react.hooks._
import crystal.react.reuse._
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.GuideStarCandidate
import explore.model.ObsConfiguration
import explore.model.PosAngle
import explore.model.ScienceMode
import explore.model.TargetVisualOptions
import explore.model.enum.Visible
import explore.model.reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.geom.jts.interpreter._
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.model.SiderealTracking
import lucuma.svgdotjs.Svg
import lucuma.ui.reusability._
import org.scalajs.dom.Element
import org.scalajs.dom.document
import react.aladin._
import react.common._
import react.common.implicits._
import react.resizeDetector.hooks._
import react.semanticui.elements.button.Button
import react.semanticui.sizes._

import java.time.LocalDate
import java.time.ZoneId
import scala.concurrent.duration._

final case class AladinContainer(
  target:                 ReuseView[SiderealTracking],
  obsConf:                Option[ObsConfiguration],
  scienceMode:            Option[ScienceMode],
  options:                TargetVisualOptions,
  updateMouseCoordinates: Coordinates ==> Callback,
  updateFov:              Fov ==> Callback, // TODO Move the functionality of saving the FOV in ALadincell here
  updateViewOffset:       Offset ==> Callback,
  centerOnTarget:         ReuseView[Boolean],
  guideStarCandidates:    List[GuideStarCandidate]
) extends ReactFnProps[AladinContainer](AladinContainer.component)

object AladinContainer {

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
      // Base coordinates with pm correction if possible
      .useMemoBy(_.obsConf.map(_.obsInstant)) { p => i =>
        i.flatMap(p.target.get.at).getOrElse(p.target.get.baseCoordinates)
      }
      // View coordinates base coordinates with pm correction if possible + user panning
      .useStateBy { (p, baseCoordinates) =>
        baseCoordinates.value.offsetBy(Angle.Angle0, p.options.viewOffset)
      }
      // Memoized svg
      .useMemoBy((p, _, _) => (p.scienceMode, p.obsConf.map(_.posAngle), p.options)) {
        case (_, _, _) => { case (mode, posAngle, options) =>
          val pa = posAngle
            .collect {
              case PosAngle.Fixed(a)               => a
              case PosAngle.AllowFlip(a)           => a
              case PosAngle.ParallacticOverride(a) => a
            }

          val candidatesVisibility =
            ExploreStyles.GuideStarCandidateVisible.when_(options.agsCandidates.visible)

          val shapes = pa
            .map { posAngle =>
              GmosGeometry.shapesForMode(posAngle, mode) ++
                GmosGeometry.commonShapes(posAngle, candidatesVisibility)
            }
            .getOrElse(
              GmosGeometry.commonShapes(Angle.Angle0, candidatesVisibility)
            )

          visualization.shapesToSvg(shapes, GmosGeometry.pp, GmosGeometry.ScaleFactor)
        }
      }
      // Ref to the aladin component
      .useRefToScalaComponent(AladinComp)
      // If needed center on target
      .useEffectWithDepsBy((p, baseCoordinates, _, _, _) =>
        (baseCoordinates.value, p.centerOnTarget.get)
      )((_, _, _, _, aladinRef) => { case (coords, center) =>
        aladinRef.get.asCBO
          .flatMapCB(
            _.backend.gotoRaDec(coords.ra.toAngle.toDoubleDegrees,
                                coords.dec.toAngle.toSignedDoubleDegrees
            )
          )
          .when(center)
      })
      // Function to calculate coordinates
      .useSerialState(DefaultWorld2PixFn)
      // resize detector
      .useResizeDetector()
      // Update the world2pix function
      .useEffectWithDepsBy { (p, _, currentPos, _, aladinRef, _, resize) =>
        (resize, p.options, currentPos, aladinRef)
      } { (_, _, _, _, aladinRef, w, _) => _ =>
        aladinRef.get.asCBO.flatMapCB(_.backend.world2pixFn.flatMap(w.setState))
      }
      // Render the visualization, only if current pos, fov or size changes
      .useEffectWithDepsBy((p, baseCoordinates, currentPos, _, _, world2pix, resize) =>
        (p.options.fovAngle,
         p.options.agsCandidates,
         p.scienceMode,
         p.obsConf.map(_.posAngle),
         currentPos,
         world2pix.value(baseCoordinates.value),
         resize
        )
      ) { (_, _, _, svg, aladinRef, _, _) =>
        { case (_, _, _, _, _, off, _) =>
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
      // memoized catalog targets with their proper motions corrected
      .useMemoBy((props, _, _, _, _, _, _) =>
        (props.guideStarCandidates,
         props.options.agsCandidates.visible,
         props.obsConf.isDefined,
         props.obsConf.map(_.obsInstant)
        )
      ) { (_, _, _, _, _, _, _) =>
        { case (candidates, visible, confPresent, obsInstant) =>
          if (confPresent) {
            val candidatesVisibility =
              ExploreStyles.GuideStarCandidateVisible.when_(visible)

            obsInstant.foldMap { obsInstant =>
              candidates.flatMap { g =>
                val targetEpoch        = g.tracking.epoch.epochYear.round
                // Approximate to the midddle of the yaer
                val targetEpochInstant =
                  LocalDate.of(targetEpoch.toInt, 6, 1).atStartOfDay(ZoneId.of("UTC")).toInstant()

                (g.tracking
                   .at(targetEpochInstant),
                 g.tracking
                   .at(obsInstant)
                ).mapN { (source, dest) =>
                  List[SVGTarget](SVGTarget.GuideStarCandidateTarget(dest, candidatesVisibility, 3),
                                  SVGTarget.LineTo(
                                    source,
                                    dest,
                                    ExploreStyles.PMGSCorrectionLine |+| candidatesVisibility
                                  )
                  )
                }
              }.flatten
            }
          } else Nil
        }
      }
      .renderWithReuse {
        (props, baseCoordinates, currentPos, _, aladinRef, world2pix, resize, candidates) =>
          /**
           * Called when the position changes, i.e. aladin pans. We want to offset the visualization
           * to keep the internal target correct
           */
          def onPositionChanged(u: PositionChanged): Callback = {
            val viewCoords = Coordinates(u.ra, u.dec)
            val viewOffset = baseCoordinates.value.diff(viewCoords).offset
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
              .getOrElse(Coordinates.fromHmsDms.reverseGet(baseCoordinates.value))

          val showBase = props.obsConf.isDefined

          val overlayTargets = if (showBase) {
            List(
              SVGTarget.CrosshairTarget(baseCoordinates.value, ExploreStyles.ScienceTarget, 10),
              SVGTarget.CircleTarget(props.target.get.baseCoordinates, ExploreStyles.BaseTarget, 3),
              SVGTarget.LineTo(baseCoordinates.value,
                               props.target.get.baseCoordinates,
                               ExploreStyles.PMCorrectionLine
              )
            )
          } else
            List(
              SVGTarget.CrosshairTarget(baseCoordinates.value, ExploreStyles.ScienceTarget, 10)
            )

          <.div(
            ExploreStyles.AladinContainerBody,
            // This is a bit tricky. Sometimes the height can be 0 or a very low number.
            // This happens during a second render. If we let the height to be zero, aladin
            // will take it as 1. This height ends up being a denominator, which, if low,
            // will make aladin request a large amount of tiles and end up freeze explore.
            if (resize.height.exists(_ >= 100)) {
              ReactFragment(
                <.div(
                  ExploreStyles.AladinZoomControl,
                  Button(size = Small,
                         icon = true,
                         onClick = aladinRef.get.asCBO.flatMapCB(_.backend.increaseZoom).toCallback
                  )(
                    ExploreStyles.ButtonOnAladin,
                    Icons.ThinPlus
                  ),
                  Button(size = Small,
                         icon = true,
                         onClick = aladinRef.get.asCBO.flatMapCB(_.backend.decreaseZoom).toCallback
                  )(
                    ExploreStyles.ButtonOnAladin,
                    Icons.ThinMinus
                  )
                ),
                (resize.width, resize.height)
                  .mapN(
                    SVGTargetsOverlay(_,
                                      _,
                                      world2pix.value.reuseNever,
                                      overlayTargets ++ candidates
                    )
                  ),
                AladinComp.withRef(aladinRef) {
                  Aladin(
                    ExploreStyles.TargetAladin,
                    showReticle = false,
                    showLayersControl = false,
                    target = baseCoordinatesForAladin,
                    fov = props.options.fovAngle,
                    showGotoControl = false,
                    showZoomControl = false,
                    customize = includeSvg _
                  )
                }
              )
            } else EmptyVdom
          )
            .withRef(resize.ref)
      }
}
