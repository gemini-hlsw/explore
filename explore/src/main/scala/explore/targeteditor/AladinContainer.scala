// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.all._
import crystal.react.View
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.ObsConfiguration
import explore.model.TargetVisualOptions
import explore.model.enums.Visible
import explore.model.reusability._
import explore.visualization._
import japgolly.scalajs.react.Reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ags.AgsAnalysis
import lucuma.ags.GuideStarCandidate
import lucuma.core.enums.PortDisposition
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.SiderealTracking
import lucuma.ui.reusability._
import org.scalajs.dom.Element
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
  target:                 View[SiderealTracking],
  obsConf:                ObsConfiguration,
  options:                TargetVisualOptions,
  updateMouseCoordinates: Coordinates => Callback,
  updateFov:              Fov => Callback, // TODO Move the functionality of saving the FOV in ALadincell here
  updateViewOffset:       Offset => Callback,
  centerOnTarget:         View[Boolean],
  selectedGuideStarIndex: Int,
  guideStarCandidates:    List[(GuideStarCandidate, AgsAnalysis)]
) extends ReactFnProps[AladinContainer](AladinContainer.component)

object AladinContainer {

  type Props       = AladinContainer
  type World2PixFn = Coordinates => Option[(Double, Double)]
  val DefaultWorld2PixFn: World2PixFn = (_: Coordinates) => None

  // This is used for screen coordinates, thus it doesn't need a lot of precission
  implicit val doubleReuse = Reusability.double(1.0)

  val AladinComp = Aladin.component

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
      .useMemoBy(_.obsConf.vizTime) { p => i =>
        p.target.get.at(i).getOrElse(p.target.get.baseCoordinates)
      }
      // View coordinates base coordinates with pm correction + user panning
      .useStateBy { (p, baseCoordinates) =>
        baseCoordinates.value.offsetBy(Angle.Angle0, p.options.viewOffset)
      }
      // Selected guide star
      .useMemoBy((p, _, _) => (p.guideStarCandidates, p.selectedGuideStarIndex)) {
        case (_, _, _) => { case (candidates, selectedIndex) =>
          candidates.lift(selectedIndex).filter(_._2.isUsable)
        }
      }
      // Ref to the aladin component
      .useRefToScalaComponent(AladinComp)
      // Memoized svg
      .useMemoBy((p, _, _, gs, _) =>
        (p.obsConf.scienceMode, p.obsConf.posAngleConstraint, p.options, gs.value)
      ) {
        case (_, baseCoordinates, _, _, _) => { case (mode, posAngle, options, gs) =>
          val pa = posAngle
            .collect {
              case PosAngleConstraint.Fixed(a)               => a
              case PosAngleConstraint.AllowFlip(a)           => a
              case PosAngleConstraint.ParallacticOverride(a) => a
            }

          val candidatesVisibility =
            ExploreStyles.GuideStarCandidateVisible.when_(options.agsCandidates.visible)

          val probeArmShapes = (gs, pa).mapN { case ((c, _), posAngle) =>
            val gsOffset = baseCoordinates.diff(c.tracking.baseCoordinates).offset
            GmosGeometry.probeShapes(posAngle,
                                     gsOffset,
                                     Offset.Zero,
                                     mode,
                                     PortDisposition.Side,
                                     Css.Empty
            )
          }

          val shapes = pa
            .map { posAngle =>
              val baseShapes =
                GmosGeometry.shapesForMode(posAngle, mode, PortDisposition.Side) ++
                  GmosGeometry.commonShapes(posAngle, candidatesVisibility)

              probeArmShapes
                .map { probeArm =>
                  baseShapes ++ probeArm
                }
                .getOrElse(baseShapes)
            }
            .getOrElse(
              GmosGeometry.commonShapes(Angle.Angle0, candidatesVisibility)
            )
          shapes
        }
      }
      // If needed center on target
      .useEffectWithDepsBy((p, baseCoordinates, _, _, _, _) =>
        (baseCoordinates.value, p.centerOnTarget.get)
      )((_, _, _, _, aladinRef, _) => { case (coords, center) =>
        aladinRef.get.asCBO
          .flatMapCB(
            _.backend.gotoRaDec(coords.ra.toAngle.toDoubleDegrees,
                                coords.dec.toAngle.toSignedDoubleDegrees
            )
          )
          .when(center)
      })
      // resize detector
      .useResizeDetector()
      // memoized catalog targets with their proper motions corrected
      .useMemoBy((props, _, _, selectedGs, _, _, _) =>
        (props.guideStarCandidates,
         props.options.agsCandidates.visible,
         props.obsConf.hasPosAngleConstraint,
         props.obsConf.vizTime,
         selectedGs
        )
      ) { (_, _, _, _, _, _, _) =>
        { case (candidates, visible, confPresent, obsInstant, selectedGS) =>
          if (confPresent) {
            val candidatesVisibility =
              ExploreStyles.GuideStarCandidateVisible.when_(visible)

            val selectedGSTarget = selectedGS
              .flatMap { case (c, _) => c.tracking.at(obsInstant) }
              .map(c => SVGTarget.GuideStarTarget(c, Css.Empty, 5))

            candidates
              .filterNot(x => selectedGS.exists(_._1 === x._1))
              .flatMap { case (g, _) =>
                val targetEpoch        = g.tracking.epoch.epochYear.round
                // Approximate to the midddle of the yaer
                val targetEpochInstant =
                  LocalDate.of(targetEpoch.toInt, 6, 1).atStartOfDay(ZoneId.of("UTC")).toInstant()

                (g.tracking
                   .at(targetEpochInstant),
                 g.tracking.at(obsInstant)
                ).mapN { (source, dest) =>
                  List[SVGTarget](
                    SVGTarget.GuideStarCandidateTarget(dest, candidatesVisibility, 3),
                    SVGTarget.LineTo(
                      source,
                      dest,
                      ExploreStyles.PMGSCorrectionLine |+| candidatesVisibility
                    )
                  )
                }
              }
              .flatten ++ selectedGSTarget.toList
          } else Nil
        }
      }
      // Use fov from aladin
      .useState(none[Fov])
      .render {
        (props, baseCoordinates, currentPos, _, aladinRef, vizShapes, resize, candidates, fov) =>
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

          def onZoom = (v: Fov) => fov.setState(v.some) *> props.updateFov(v)

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

          val baseTargets =
            List(
              SVGTarget.CrosshairTarget(baseCoordinates.value, ExploreStyles.ScienceTarget, 10),
              SVGTarget.CircleTarget(props.target.get.baseCoordinates, ExploreStyles.BaseTarget, 3),
              SVGTarget.LineTo(baseCoordinates.value,
                               props.target.get.baseCoordinates,
                               ExploreStyles.PMCorrectionLine
              )
            )

          val screenOffset =
            currentPos.value.map(_.diff(baseCoordinates).offset).getOrElse(Offset.Zero)

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
                (resize.width, resize.height, fov.value)
                  .mapN(
                    TargetsOverlay(_,
                                   _,
                                   _,
                                   screenOffset,
                                   baseCoordinates,
                                   baseTargets ++ candidates
                    )
                  ),
                (resize.width, resize.height, fov.value)
                  .mapN(
                    SVGVisualizationOverlay(
                      _,
                      _,
                      _,
                      screenOffset,
                      vizShapes
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
                    showFullscreenControl = false,
                    customize = includeSvg _
                  )
                }
              )
            } else EmptyVdom
          )
            .withRef(resize.ref)
      }
}
