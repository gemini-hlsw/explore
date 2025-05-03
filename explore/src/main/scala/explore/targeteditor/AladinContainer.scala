// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.syntax.all.*
import eu.timepit.refined.*
import eu.timepit.refined.numeric.NonNegative
import explore.aladin.AladinZoomControl
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.AladinMouseScroll
import explore.model.Asterism
import explore.model.AsterismVisualOptions
import explore.model.ConfigurationForVisualization
import explore.model.Constants
import explore.model.GlobalPreferences
import explore.model.enums.Visible
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.Reusability.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ags.AgsAnalysis
import lucuma.core.enums.GuideSpeed
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.PortDisposition
import lucuma.core.enums.SequenceType
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.model.CoordinatesAtVizTime
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.react.resizeDetector.hooks.*
import lucuma.refined.*
import lucuma.ui.aladin.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.visualization.*

import java.time.Instant
import java.time.LocalDate
import scala.concurrent.duration.*

case class AladinContainer(
  asterism:               Asterism,
  obsTime:                Instant,
  vizConf:                Option[ConfigurationForVisualization],
  globalPreferences:      GlobalPreferences,
  options:                AsterismVisualOptions,
  updateMouseCoordinates: Coordinates => Callback,
  updateFov:              Fov => Callback,
  updateViewOffset:       Offset => Callback,
  selectedGuideStar:      Option[AgsAnalysis.Usable],
  guideStarCandidates:    List[AgsAnalysis.Usable]
) extends ReactFnProps(AladinContainer.component):
  val siderealDiscretizedObsTime: SiderealDiscretizedObsTime =
    SiderealDiscretizedObsTime(obsTime, vizConf.flatMap(_.selectedPosAngleConstraint))

object AladinContainer extends AladinCommon {

  private type Props = AladinContainer

  // We need to detect if the selected GS deserves a refresh, this could be if the
  // selected target changes or if e.g. the pos angle change for the same target
  private given Reusability[AgsAnalysis.Usable] =
    Reusability.by(u => (u.target, u.posAngle))

  private given Reusability[List[AgsAnalysis.Usable]] = Reusability.by(_.length)

  private def speedCss(gs: GuideSpeed): Css =
    gs match
      case GuideSpeed.Fast   =>
        ExploreStyles.GuideSpeedFast
      case GuideSpeed.Medium =>
        ExploreStyles.GuideSpeedMedium
      case GuideSpeed.Slow   =>
        ExploreStyles.GuideSpeedSlow

  private def baseAndScience(p: Props) = {
    val base: CoordinatesAtVizTime = p.asterism.baseTracking
      .at(p.obsTime)
      .getOrElse(CoordinatesAtVizTime(p.asterism.baseTracking.baseCoordinates))

    val science = p.asterism.toSidereal
      .map(t =>
        (t.id === p.asterism.focus.id,
         t.target.name,
         t.target.tracking.at(p.obsTime),
         t.target.tracking.baseCoordinates
        )
      )
    (base, science)
  }

  private val component =
    ScalaFnComponent[Props]: props =>
      for {
        // Base coordinates and science targets with pm correction if possible
        baseCoords <- useState(baseAndScience(props))
        // View coordinates base coordinates with pm correction + user panning
        currentPos <-
          useState(baseCoords.value._1.value.offsetBy(Angle.Angle0, props.options.viewOffset))
        // Update coordinates if asterism or obsTime changes
        _          <- useEffectWithDeps((props.asterism, props.obsTime)): _ =>
                        val (base, science) = baseAndScience(props)
                        baseCoords.setState((base, science)) *>
                          currentPos.setState:
                            base.value.offsetBy(Angle.Angle0, props.options.viewOffset)
        // Ref to the aladin component
        aladinRef  <- useState(none[Aladin])
        // If view offset changes upstream to zero, redraw
        _          <-
          useEffectWithDeps((baseCoords, props.options.viewOffset)): (_, offset) =>
            val newCoords = baseCoords.value._1.value.offsetBy(Angle.Angle0, offset)
            newCoords
              .map: coords =>
                aladinRef.value
                  .traverse(_.gotoRaDecCB(coords))
                  .void
                  .when_(offset === Offset.Zero)
              .getOrEmpty
        // Memoized svg for visualization shapes
        shapes     <-
          useMemo(
            (baseCoords, props.vizConf, props.globalPreferences.agsOverlay, props.selectedGuideStar)
          ) { _ =>
            val candidatesVisibilityCss =
              ExploreStyles.GuideStarCandidateVisible.when_(props.globalPreferences.agsOverlay)

            props.vizConf.map(_.configuration.obsModeType).flatMap {
              case ObservingModeType.Flamingos2LongSlit                                      =>
                Flamingos2Geometry.f2Geometry(
                  baseCoords.value._1.value,
                  props.vizConf.flatMap(_.scienceOffsets),
                  props.vizConf.flatMap(_.acquisitionOffsets),
                  props.vizConf.map(_.posAngle),
                  props.vizConf.map(_.configuration),
                  PortDisposition.Side,
                  props.selectedGuideStar,
                  candidatesVisibilityCss
                )
              case ObservingModeType.GmosNorthLongSlit | ObservingModeType.GmosSouthLongSlit =>
                GmosGeometry.gmosGeometry(
                  baseCoords.value._1.value,
                  props.vizConf.flatMap(_.scienceOffsets),
                  props.vizConf.flatMap(_.acquisitionOffsets),
                  props.vizConf.map(_.posAngle),
                  props.vizConf.map(_.configuration),
                  PortDisposition.Side,
                  props.selectedGuideStar,
                  candidatesVisibilityCss
                )
            }
          }
        // resize detector
        resize     <- useResizeDetector
        // memoized catalog targets with their proper motions corrected
        candidates <- useMemo(
                        (props.guideStarCandidates,
                         props.globalPreferences.showCatalog,
                         props.globalPreferences.fullScreen,
                         props.options.fovRA,
                         props.siderealDiscretizedObsTime,
                         props.vizConf.map(_.configuration),
                         props.selectedGuideStar,
                         baseCoords
                        )
                      ):
                        (
                          candidates,
                          visible,
                          _,
                          fovRA,
                          siderealDiscretizedObsTime,
                          configuration,
                          selectedGS,
                          baseCoords
                        ) =>
                          selectedGS.posAngle.foldMap: _ =>
                            val (baseCoordinates, scienceTargets) = baseCoords.value

                            val fov = fovRA.toMicroarcseconds / 1e6

                            def calcSize(size: Double): Double = size.max(size * (225 / fov))

                            val candidatesVisibility =
                              ExploreStyles.GuideStarCandidateVisible.when_(visible)

                            candidates
                              // TODO This should be done in AGS proper
                              .filterNot(x =>
                                scienceTargets.contains(x.target.tracking.baseCoordinates)
                              )
                              .flatMap { g =>
                                val tracking           = g.target.tracking
                                val targetEpoch        = tracking.epoch.epochYear.round
                                // Approximate to the midddle of the year
                                val targetEpochInstant =
                                  LocalDate
                                    .of(targetEpoch.toInt, 6, 1)
                                    .atStartOfDay(Constants.UTC)
                                    .toInstant()

                                val candidateCss =
                                  if (configuration.isEmpty) Css.Empty else speedCss(g.guideSpeed)

                                (tracking.at(targetEpochInstant),
                                 tracking.at(siderealDiscretizedObsTime.obsTime)
                                )
                                  .mapN { (source, dest) =>
                                    if (candidates.length < 500) {
                                      List[SVGTarget](
                                        if (selectedGS.forall(_.target.id === g.target.id)) {
                                          SVGTarget.GuideStarTarget(dest,
                                                                    candidateCss,
                                                                    calcSize(4),
                                                                    g
                                          )
                                        } else {
                                          SVGTarget.GuideStarCandidateTarget(
                                            dest,
                                            candidateCss |+| candidatesVisibility,
                                            calcSize(3),
                                            g
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
                                          SVGTarget.GuideStarTarget(dest,
                                                                    candidateCss,
                                                                    calcSize(4),
                                                                    g
                                          )
                                        } else {
                                          SVGTarget.GuideStarCandidateTarget(
                                            dest,
                                            ExploreStyles.GuideStarCandidateCrowded |+| candidateCss |+| candidatesVisibility,
                                            calcSize(2.7),
                                            g
                                          )
                                        }
                                      )
                                    }
                                  }
                              }
                              .flatten

        // Use fov from aladin
        fov        <- useState(none[Fov])
        // Update aladin if coords change
        _          <-
          useEffectWithDeps(
            (props.globalPreferences.showCatalog, props.globalPreferences.aladinMouseScroll, resize)
          ): _ =>
            aladinRef.value.traverse(_.fixLayoutDimensionsCB).void
      } yield {
        val (baseCoordinates, scienceTargets) = baseCoords.value

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

        val includeSvg: Aladin => Callback = (v: Aladin) =>
          aladinRef.setState(v.some) *>
            v.onZoomCB(onZoom) *> // re render on zoom
            v.onPositionChangedCB(onPositionChanged) *>
            v.onMouseMoveCB(s =>
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
            SVGTarget.LineTo(
              baseCoordinates.value,
              props.asterism.baseTracking.baseCoordinates,
              ExploreStyles.PMCorrectionLine
            )
          )

        val sciencePositions =
          if (scienceTargets.length > 1)
            scienceTargets.flatMap { (selected, name, pm, base) =>
              pm.foldMap { pm =>
                List(
                  SVGTarget.ScienceTarget(
                    pm,
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
          f:       ConfigurationForVisualization => Option[NonEmptyList[Offset]],
          oType:   SequenceType,
          css:     Css,
          visible: Visible
        ) =
          props.vizConf.foldMap(f).foldMap(_.toList).zipWithIndex.map { case (o, i) =>
            for {
              idx <- refineV[NonNegative](i).toOption
              gs  <- props.selectedGuideStar
              c   <- baseCoordinates.value.offsetBy(gs.posAngle, o) if visible
            } yield SVGTarget.OffsetIndicator(c, idx, o, oType, css, 4)
          }

        val scienceOffsetIndicators =
          offsetIndicators(
            _.scienceOffsets,
            SequenceType.Science,
            ExploreStyles.ScienceOffsetPosition,
            props.globalPreferences.scienceOffsets
          )

        val acquisitionOffsetIndicators =
          offsetIndicators(
            _.acquisitionOffsets,
            SequenceType.Acquisition,
            ExploreStyles.AcquisitionOffsetPosition,
            props.globalPreferences.acquisitionOffsets
          )

        val offsetTargets =
          // order is important, scienc to be drawn above acq
          (acquisitionOffsetIndicators |+| scienceOffsetIndicators).flattenOption

        val screenOffset =
          currentPos.value.map(_.diff(baseCoordinates.value).offset).getOrElse(Offset.Zero)

        <.div.withRef(resize.ref)(ExploreStyles.AladinContainerBody)(
          // This is a bit tricky. Sometimes the height can be 0 or a very low number.
          // This happens during a second render. If we let the height to be zero, aladin
          // will take it as 1. This height ends up being a denominator, which, if low,
          // will make aladin request a large amount of tiles and end up freeze explore.
          if (resize.height.exists(_ >= 100)) {
            ReactFragment(
              aladinRef.value.map(AladinZoomControl(_)),
              HelpIcon("aladin-cell.md".refined, ExploreStyles.AladinHelpIcon),
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
              (resize.width, resize.height, fov.value, shapes.value.flatMap(NonEmptyMap.fromMap))
                .mapN(
                  SVGVisualizationOverlay(
                    _,
                    _,
                    _,
                    screenOffset,
                    _
                  )
                ),
              ReactAladin(
                ExploreStyles.TargetAladin |+| ExploreStyles.TargetAladinDisableMouse
                  .unless_(props.globalPreferences.aladinMouseScroll.value),
                AladinOptions(
                  showReticle = false,
                  showLayersControl = false,
                  target = baseCoordinatesForAladin,
                  fov = Angle.fromMicroarcseconds(
                    props.options.fovDec.toMicroarcseconds
                      .max(props.options.fovRA.toMicroarcseconds)
                  ),
                  showGotoControl = false,
                  showZoomControl = false,
                  showProjectionControl = false,
                  showSimbadPointerControl = false,
                  showFullscreenControl = false,
                  showCooLocation = false,
                  showFov = false
                ),
                customize = includeSvg
              )
            )
          } else EmptyVdom
        )
      }
}
