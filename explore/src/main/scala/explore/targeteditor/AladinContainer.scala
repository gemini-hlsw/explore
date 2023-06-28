// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all.*
import crystal.react.*
import crystal.react.given
import crystal.react.reuse.*
import eu.timepit.refined.*
import eu.timepit.refined.numeric.NonNegative
import explore.Icons
import explore.aladin.AladinZoomControl
import explore.components.ui.ExploreStyles
import explore.components.ui.ExploreStyles.ExploreTable
import explore.model.AladinMouseScroll
import explore.model.Asterism
import explore.model.Constants
import explore.model.ObsConfiguration
import explore.model.TargetVisualOptions
import explore.model.UserGlobalPreferences
import explore.model.enums.Visible
import explore.model.reusability.given
import explore.visualization.*
import japgolly.scalajs.react.Reusability.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ags.AgsAnalysis
import lucuma.ags.AgsPosition
import lucuma.core.enums.GuideSpeed
import lucuma.core.enums.PortDisposition
import lucuma.core.enums.SequenceType
import lucuma.core.geom.Area
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.model.CoordinatesAtVizTime
import lucuma.core.model.SiderealTracking
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import org.scalajs.dom.Element
import react.aladin.*
import react.common.Css
import react.common.ReactFnProps
import react.primereact.Button
import react.resizeDetector.hooks.*

import java.time.Instant
import java.time.LocalDate
import java.time.ZoneId
import scala.collection.immutable.SortedMap
import scala.concurrent.duration.*

case class AladinContainer(
  asterism:               Asterism,
  vizTime:                Instant,
  obsConf:                Option[ObsConfiguration],
  userPreferences:        UserGlobalPreferences,
  options:                TargetVisualOptions,
  updateMouseCoordinates: Coordinates => Callback,
  updateFov:              Fov => Callback,
  updateViewOffset:       Offset => Callback,
  selectedGuideStar:      Option[AgsAnalysis],
  guideStarCandidates:    List[AgsAnalysis]
) extends ReactFnProps(AladinContainer.component)

object AladinContainer extends AladinCommon {

  private type Props = AladinContainer

  // This is used for screen coordinates, thus it doesn't need a lot of precission
  private given Reusability[Double]              = Reusability.double(1.0)
  // We need to dectect if the selected GS deserves a refresh, this could be if the
  // selected target changes or if e.g. the pos angle change for the same target
  private given Reusability[Option[AgsAnalysis]] = Reusability.by(_ match {
    case Some(AgsAnalysis.Usable(_, target, _, _, v)) => ((target.id, v)).some
    // simulate vignetting for reusability it only matters if it changes
    case Some(t)                                      =>
      (t.target.id, NonEmptyList.of((AgsPosition(Angle.Angle0, Offset.Zero), Area.MaxArea))).some
    case _                                            => None
  })
  private given Reusability[List[AgsAnalysis]]   = Reusability.by(_.length)
  private given Reusability[Props]               =
    Reusability.by(x => (x.asterism, x.obsConf, x.userPreferences, x.options))
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

  private def baseAndScience(p: Props) = {
    val base: CoordinatesAtVizTime = p.asterism
      .baseTrackingAt(p.vizTime)
      .flatMap(_.at(p.vizTime))
      .getOrElse(CoordinatesAtVizTime(p.asterism.baseTracking.baseCoordinates))

    val science = p.asterism.toSidereal
      .map(t =>
        (t.id === p.asterism.focus.id,
         t.target.name,
         t.target.tracking.at(p.vizTime),
         t.target.tracking.baseCoordinates
        )
      )
    (base, science)
  }

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      // Base coordinates and science targets with pm correction if possible
      .useStateBy(p => baseAndScience(p))
      // View coordinates base coordinates with pm correction + user panning
      .useStateBy { (p, baseCoordinates) =>
        baseCoordinates.value._1.value.offsetBy(Angle.Angle0, p.options.viewOffset)
      }
      .useEffectWithDepsBy((p, _, _) => (p.asterism, p.vizTime)) {
        (p, baseCoordinates, currentPos) => _ =>
          val (base, science) = baseAndScience(p)
          baseCoordinates.setState((base, science)) *>
            currentPos.setState(
              base.value.offsetBy(Angle.Angle0, p.options.viewOffset)
            )
      }
      // Ref to the aladin component
      .useRefToScalaComponent(AladinComp)
      // If view offset changes upstream to zero, redraw
      .useEffectWithDepsBy((p, baseCoordinates, _, _) => (baseCoordinates, p.options.viewOffset)) {
        (_, baseCoordinates, viewCoordinates, aladinRef) => (_, offset) =>
          {
            val newCoords = baseCoordinates.value._1.value.offsetBy(Angle.Angle0, offset)
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
        (allCoordinates, p.obsConf, p.userPreferences, p.selectedGuideStar)
      ) {
        (_, _, _, _) => (
          allCoordinates,
          configuration,
          userPrefs,
          gs
        ) =>
          val candidatesVisibilityCss =
            ExploreStyles.GuideStarCandidateVisible.when_(userPrefs.aladinAgsOverlay.isVisible)

          GmosGeometry.gmosGeometry(
            allCoordinates.value._1.value,
            configuration,
            PortDisposition.Side,
            gs,
            candidatesVisibilityCss
          )

      }
      // resize detector
      .useResizeDetector()
      // memoized catalog targets with their proper motions corrected
      .useMemoBy((props, allCoordinates, _, _, _, _) =>
        (props.guideStarCandidates,
         props.userPreferences.aladinShowCatalog.isVisible,
         props.userPreferences.fullScreen,
         props.options.fovRA,
         props.vizTime,
         props.obsConf.flatMap(_.configuration),
         props.selectedGuideStar,
         allCoordinates
        )
      ) { case (_, _, _, _, _, _) =>
        (
          candidates,
          visible,
          _,
          fovRA,
          obsInstant,
          configuration,
          selectedGS,
          allCoordinates
        ) =>
          selectedGS.posAngle
            .map { posAngle =>
              val (baseCoordinates, scienceTargets) = allCoordinates.value

              val fov = fovRA.toMicroarcseconds / 1e6

              def calcSize(size: Double): Double = size.max(size * (225 / fov))

              val candidatesVisibility =
                ExploreStyles.GuideStarCandidateVisible.when_(visible)

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
                      .atStartOfDay(Constants.UTC)
                      .toInstant()

                  val vignettesScience = g match
                    case AgsAnalysis.VignettesScience(_, _) => true
                    case _                                  => false

                  val candidateCss = g.match
                    case _ if configuration.isEmpty                           =>
                      // Don't color the stars for guide speed if there is no mode selected
                      Css.Empty
                    case AgsAnalysis.Usable(_, _, Some(s), _, _)              =>
                      speedCss(s)
                    case AgsAnalysis.NotReachableAtPosition(_, _, Some(s), _) =>
                      speedCss(s)
                    case m                                                    =>
                      ExploreStyles.VignettedGS

                  (tracking.at(targetEpochInstant), tracking.at(obsInstant)).mapN {
                    (source, dest) =>
                      val offset = baseCoordinates.value.diff(dest).offset
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
            val viewOffset = baseCoordinates.value.diff(viewCoords).offset
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

          val vizTime = props.vizTime

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
              .getOrElse(Coordinates.fromHmsDms.reverseGet(baseCoordinates.value))

          val basePosition =
            List(
              SVGTarget.CrosshairTarget(baseCoordinates.value, Css.Empty, 10),
              SVGTarget.CircleTarget(baseCoordinates.value, ExploreStyles.BaseTarget, 3),
              SVGTarget.LineTo(baseCoordinates.value,
                               props.asterism.baseTracking.baseCoordinates,
                               ExploreStyles.PMCorrectionLine
              )
            )

          val sciencePositions        =
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

          def offsetIndicators(
            f:       ObsConfiguration => Option[NonEmptyList[Offset]],
            oType:   SequenceType,
            css:     Css,
            visible: Visible
          ) =
            props.obsConf.foldMap(f).foldMap(_.toList).zipWithIndex.map { case (o, i) =>
              for {
                idx <- refineV[NonNegative](i).toOption
                gs  <- props.selectedGuideStar
                pa  <- gs.posAngle
                c   <- baseCoordinates.value.offsetBy(pa, o)
                if visible.isVisible
              } yield SVGTarget.OffsetIndicator(c, idx, o, oType, css, 4)
            }
          val scienceOffsetIndicators =
            offsetIndicators(_.scienceOffsets,
                             SequenceType.Science,
                             ExploreStyles.ScienceOffsetPosition,
                             props.userPreferences.aladinScienceOffsets
            )

          val acquisitionOffsetIndicators =
            offsetIndicators(_.acquisitionOffsets,
                             SequenceType.Acquisition,
                             ExploreStyles.AcquisitionOffsetPosition,
                             props.userPreferences.aladinAcquisitionOffsets
            )

          val offsetTargets =
            // order is important, scienc to be drawn above acq
            (acquisitionOffsetIndicators |+| scienceOffsetIndicators).flattenOption

          val screenOffset =
            currentPos.value.map(_.diff(baseCoordinates.value).offset).getOrElse(Offset.Zero)

          val key =
            s"aladin-${resize.width}-${resize.height}-${props.userPreferences.aladinShowCatalog}"

          <.div(
            ExploreStyles.AladinContainerBody,
            ^.key := key,
            // This is a bit tricky. Sometimes the height can be 0 or a very low number.
            // This happens during a second render. If we let the height to be zero, aladin
            // will take it as 1. This height ends up being a denominator, which, if low,
            // will make aladin request a large amount of tiles and end up freeze explore.
            if (resize.height.exists(_ >= 100)) {
              ReactFragment(
                AladinZoomControl(aladinRef),
                (resize.width, resize.height, fov.value)
                  .mapN(
                    TargetsOverlay(
                      _,
                      _,
                      _,
                      screenOffset,
                      baseCoordinates.value,
                      // Order matters
                      candidates ++ basePosition ++ sciencePositions ++ offsetTargets
                    )
                  ),
                (resize.width,
                 resize.height,
                 fov.value,
                 vizShapes.value.flatMap(NonEmptyMap.fromMap)
                )
                  .mapN(
                    SVGVisualizationOverlay(
                      _,
                      _,
                      _,
                      screenOffset,
                      _
                    )
                  ),
                AladinComp
                  .withRef(aladinRef) {
                    Aladin(
                      ExploreStyles.TargetAladin |+| ExploreStyles.TargetAladinDisableMouse
                        .unless_(props.userPreferences.aladinMouseScroll.value),
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
