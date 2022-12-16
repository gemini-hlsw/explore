// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.all.*
import crystal.react.View
import crystal.react.implicits.*
import crystal.react.reuse.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.AladinMouseScroll
import explore.model.Asterism
import explore.model.ObsConfiguration
import explore.model.TargetVisualOptions
import explore.model.enums.Visible
import explore.model.reusability.*
import explore.model.reusability.given
import explore.visualization.*
import japgolly.scalajs.react.Reusability.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ags.AgsAnalysis
import lucuma.core.enums.GuideSpeed
import lucuma.core.enums.PortDisposition
import lucuma.core.geom.jts.interpreter.*
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.model.SiderealTracking
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import org.scalajs.dom.Element
import react.aladin.*
import react.common.Css
import react.common.ReactFnProps
import react.resizeDetector.hooks.*
import react.semanticui.elements.button.Button
import react.semanticui.sizes.*

import java.time.LocalDate
import java.time.ZoneId
import scala.concurrent.duration.*

case class AladinContainer(
  asterism:               Asterism,
  obsConf:                ObsConfiguration,
  allowMouseScroll:       AladinMouseScroll,
  options:                TargetVisualOptions,
  updateMouseCoordinates: Coordinates => Callback,
  // TODO Move the functionality of saving the FOV in ALadincell here
  updateFov:              Fov => Callback,
  updateViewOffset:       Offset => Callback,
  selectedGuideStar:      Option[AgsAnalysis],
  guideStarCandidates:    List[AgsAnalysis]
) extends ReactFnProps(AladinContainer.component)

object AladinContainer extends AladinCommon {

  private type Props = AladinContainer

  // This is used for screen coordinates, thus it doesn't need a lot of precission
  private given Reusability[Double]              = Reusability.double(1.0)
  private given Reusability[Option[AgsAnalysis]] = Reusability.by(_.map(_.target.id))
  private given Reusability[List[AgsAnalysis]]   = Reusability.by(_.length)
  private given Reusability[Props]               =
    Reusability.by(x => (x.asterism, x.obsConf, x.allowMouseScroll, x.options))
  private given Reusability[Fov]                 = Reusability.by(x => (x.y, x.y))

  private val AladinComp = Aladin.component

  private def toggleVisibility(g: Element, selector: String, option: Visible): Unit =
    g.querySelectorAll(selector).foreach {
      case e: Element =>
        option.fold(
          e.classList.remove("visualization-display"),
          e.classList.add("visualization-display")
        )
      case null       => ()
    }

  private def speedCss(gs: GuideSpeed): Css =
    gs match
      case GuideSpeed.Fast   =>
        ExploreStyles.GuideSpeedFast
      case GuideSpeed.Medium =>
        ExploreStyles.GuideSpeedMedium
      case GuideSpeed.Slow   =>
        ExploreStyles.GuideSpeedSlow

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      // Base coordinates and science targets with pm correction if possible
      .useMemoBy(p => (p.asterism, p.obsConf.vizTime)) { p => (_, i) =>
        val base    = p.asterism.baseTracking
          .at(i)
          .map(_.value)
          .getOrElse(p.asterism.baseTracking.baseCoordinates)
        val science = p.asterism.toSidereal
          .map(t =>
            (t.id === p.asterism.focus.id,
             t.target.name,
             t.target.tracking.at(i),
             t.target.tracking.baseCoordinates
            )
          )
        (base, science)
      }
      // View coordinates base coordinates with pm correction + user panning
      .useStateBy { (p, baseCoordinates) =>
        baseCoordinates.value._1.offsetBy(Angle.Angle0, p.options.viewOffset)
      }
      // Ref to the aladin component
      .useRefToScalaComponent(AladinComp)
      // If view offset changes upstream to zero, redraw
      .useEffectWithDepsBy((p, baseCoordinates, _, _) => (baseCoordinates, p.options.viewOffset)) {
        (_, baseCoordinates, viewCoordinates, aladinRef) => (_, offset) =>
          {
            val newCoords = baseCoordinates.value._1.offsetBy(Angle.Angle0, offset)
            newCoords
              .map(coords =>
                aladinRef.get.asCBO
                  .flatMapCB(
                    _.backend.gotoRaDec(coords.ra.toAngle.toDoubleDegrees,
                                        coords.dec.toAngle.toSignedDoubleDegrees
                    )
                  )
                  .asCallback
                  .void
                  .when_(offset === Offset.Zero)
              )
              .getOrEmpty
          }
      }
      // Memoized svg
      .useMemoBy((p, allCoordinates, _, _) =>
        (allCoordinates, p.obsConf.scienceMode, p.obsConf.posAngle, p.options, p.selectedGuideStar)
      ) { (_, _, _, _) => (allCoordinates, mode, posAngle, options, gs) =>
        val candidatesVisibility =
          ExploreStyles.GuideStarCandidateVisible.when_(options.agsCandidates.visible)

        val probeArmShapes = (gs, posAngle).mapN { case (c, posAngle) =>
          val gsOffset = allCoordinates._1.diff(c.target.tracking.baseCoordinates).offset
          GmosGeometry.probeShapes(posAngle,
                                   gsOffset,
                                   Offset.Zero,
                                   mode,
                                   PortDisposition.Side,
                                   Css.Empty
          )
        }

        val usableGuideStar = gs.exists(_.isUsable)

        val shapes = posAngle
          .map { posAngle =>
            val baseShapes =
              GmosGeometry.shapesForMode(posAngle, mode, PortDisposition.Side) ++
                GmosGeometry.commonShapes(posAngle, candidatesVisibility)

            probeArmShapes
              .filter(_ => usableGuideStar) // Don't show the probe if there is no usable GS
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
      // resize detector
      .useResizeDetector()
      // memoized catalog targets with their proper motions corrected
      .useMemoBy((props, allCoordinates, _, _, _, _) =>
        (props.guideStarCandidates,
         props.options.agsCandidates.visible,
         props.options.fullScreen,
         props.options.fovRA,
         props.obsConf.posAngle,
         props.obsConf.vizTime,
         props.obsConf.scienceMode,
         props.selectedGuideStar,
         allCoordinates
        )
      ) { case (_, _, _, _, _, _) =>
        (
          candidates,
          visible,
          _,
          fovRA,
          posAngle,
          obsInstant,
          scienceMode,
          selectedGS,
          allCoordinates
        ) =>
          posAngle
            .map { posAngle =>
              val (baseCoordinates, scienceTargets) = allCoordinates.value

              val fov = fovRA.toMicroarcseconds / 1e6

              def calcSize(size: Double): Double = size.max(size * (225 / fov))

              val candidatesVisibility =
                ExploreStyles.GuideStarCandidateVisible.when_(visible)

              val patrolField =
                GmosGeometry.patrolField(posAngle, scienceMode, PortDisposition.Side).map(_.eval)

              candidates
                // TODO This should be done in AGS proper
                .filterNot(x => scienceTargets.contains(x.target.tracking.baseCoordinates))
                .flatMap { g =>
                  val tracking           = g.target.tracking
                  val targetEpoch        = tracking.epoch.epochYear.round
                  // Approximate to the midddle of the year
                  val targetEpochInstant =
                    LocalDate
                      .of(targetEpoch.toInt, 6, 1)
                      .atStartOfDay(ZoneId.of("UTC"))
                      .toInstant()

                  val vignettesScience = g match
                    case AgsAnalysis.VignettesScience(_) => true
                    case _                               => false

                  val candidateCss = g.match
                    case _ if scienceMode.isEmpty                             =>
                      // Don't color the stars for guide speed if there is no mode selected
                      Css.Empty
                    case AgsAnalysis.Usable(_, _, Some(s), _, _, _)           =>
                      speedCss(s)
                    case AgsAnalysis.NotReachableAtPosition(_, _, Some(s), _) =>
                      speedCss(s)
                    case m                                                    =>
                      ExploreStyles.VignettedGS

                  (tracking.at(targetEpochInstant), tracking.at(obsInstant)).mapN {
                    (source, dest) =>
                      val offset = baseCoordinates.diff(dest).offset
                      if (candidates.length < 500) {
                        List[SVGTarget](
                          if (selectedGS.forall(_.target.id === g.target.id)) {
                            SVGTarget.GuideStarTarget(dest, candidateCss, calcSize(4))
                          } else {
                            SVGTarget.GuideStarCandidateTarget(
                              dest,
                              candidateCss |+| candidatesVisibility,
                              calcSize(3)
                            )
                          },
                          SVGTarget.LineTo(
                            source,
                            dest,
                            ExploreStyles.PMGSCorrectionLine |+| candidatesVisibility
                          )
                        )
                      } else {
                        List[SVGTarget](
                          if (selectedGS.forall(_.target.id === g.target.id)) {
                            SVGTarget.GuideStarTarget(dest, candidateCss, calcSize(4))
                          } else {
                            SVGTarget.GuideStarCandidateTarget(
                              dest,
                              ExploreStyles.GuideStarCandidateCrowded |+| candidateCss |+| candidatesVisibility,
                              calcSize(2.7)
                            )
                          }
                        )
                      }
                  }
                }
                .flatten
            }
            .getOrElse(Nil)

      }
      // Use fov from aladin
      .useState(none[Fov])
      .renderWithReuse {
        (props, allCoordinates, currentPos, aladinRef, vizShapes, resize, candidates, fov) =>
          val (baseCoordinates, scienceTargets) = allCoordinates.value

          /**
           * Called when the position changes, i.e. aladin pans. We want to offset the visualization
           * to keep the internal target correct
           */
          def onPositionChanged(u: PositionChanged): Callback = {
            val viewCoords = Coordinates(u.ra, u.dec)
            val viewOffset = baseCoordinates.diff(viewCoords).offset
            currentPos.setState(Some(viewCoords)) *>
              props.updateViewOffset(viewOffset)
          }

          def onZoom =
            (v: Fov) => {
              // Sometimes get 0 fov, ignore those
              val ignore =
                (v.x === Angle.Angle0 && v.y === Angle.Angle0) ||
                  fov.value.exists(_.isDifferentEnough(v))
              (fov.setState(v.some) *> props.updateFov(v)).unless_(ignore)
            }

          val vizTime = props.obsConf.vizTime

          def includeSvg(v: JsAladin): Callback =
            v.onZoom(onZoom) *> // re render on zoom
              v.onPositionChanged(u => onPositionChanged(u)) *>
              v.onMouseMove(s =>
                props
                  .updateMouseCoordinates(Coordinates(s.ra, s.dec))
                  .rateLimit(200.millis, 1)
                  .void
              )

          val baseCoordinatesForAladin: String =
            currentPos.value
              .map(Coordinates.fromHmsDms.reverseGet)
              .getOrElse(Coordinates.fromHmsDms.reverseGet(baseCoordinates))

          val basePosition =
            List(
              SVGTarget.CrosshairTarget(baseCoordinates, Css.Empty, 10),
              SVGTarget.CircleTarget(baseCoordinates, ExploreStyles.BaseTarget, 3),
              SVGTarget.LineTo(baseCoordinates,
                               props.asterism.baseTracking.baseCoordinates,
                               ExploreStyles.PMCorrectionLine
              )
            )

          val sciencePositions =
            if (scienceTargets.length > 1)
              scienceTargets.flatMap { (selected, name, pm, base) =>
                pm.foldMap { pm =>
                  List(
                    SVGTarget.ScienceTarget(pm,
                                            ExploreStyles.ScienceTarget,
                                            ExploreStyles.ScienceSelectedTarget,
                                            3,
                                            selected,
                                            name.value.some
                    ),
                    SVGTarget.LineTo(pm, base, ExploreStyles.PMCorrectionLine)
                  )
                }
              }
            else Nil

          val screenOffset =
            currentPos.value.map(_.diff(baseCoordinates).offset).getOrElse(Offset.Zero)

          val key = s"aladin-${resize.width}-${resize.height}-${props.allowMouseScroll.value}"

          <.div(
            ExploreStyles.AladinContainerBody,
            ^.key := key,
            // This is a bit tricky. Sometimes the height can be 0 or a very low number.
            // This happens during a second render. If we let the height to be zero, aladin
            // will take it as 1. This height ends up being a denominator, which, if low,
            // will make aladin request a large amount of tiles and end up freeze explore.
            if (resize.height.exists(_ >= 100)) {
              ReactFragment(
                <.div(
                  ExploreStyles.AladinZoomControl,
                  Button(
                    size = Small,
                    icon = true,
                    onClick = aladinRef.get.asCBO.flatMapCB(_.backend.increaseZoom).toCallback
                  )(
                    ExploreStyles.ButtonOnAladin,
                    Icons.ThinPlus
                  ),
                  Button(
                    size = Small,
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
                                   // Order matters
                                   basePosition ++ candidates ++ sciencePositions
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
                AladinComp
                  .withRef(aladinRef) {
                    Aladin(
                      ExploreStyles.TargetAladin |+| ExploreStyles.TargetAladinDisableMouse
                        .unless_(props.allowMouseScroll.value),
                      showReticle = false,
                      showLayersControl = false,
                      target = baseCoordinatesForAladin,
                      fov = Angle.fromMicroarcseconds(
                        props.options.fovDec.toMicroarcseconds
                          .max(props.options.fovRA.toMicroarcseconds)
                      ),
                      showGotoControl = false,
                      showZoomControl = false,
                      showFullscreenControl = false,
                      customize = (v: JsAladin) => includeSvg(v)
                    )
                  }
              )
            } else EmptyVdom
          )
            .withRef(resize.ref)
      }
}
