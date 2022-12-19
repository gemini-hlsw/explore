// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import boopickle.DefaultBasic.*
import cats.Order
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.PotOption
import crystal.ViewOptF
import crystal.implicits.*
import crystal.react.ReuseView
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.implicits.*
import crystal.react.reuse.*
import eu.timepit.refined.*
import eu.timepit.refined.auto.*
import explore.Icons
import explore.common.UserPreferencesQueries.*
import explore.components.ui.ExploreStyles
import explore.events.*
import explore.model.AladinFullScreen
import explore.model.AladinMouseScroll
import explore.model.AppContext
import explore.model.Asterism
import explore.model.Constants
import explore.model.ObsConfiguration
import explore.model.TargetVisualOptions
import explore.model.UserGlobalPreferences
import explore.model.WorkerClients.*
import explore.model.boopickle.Boopickle.*
import explore.model.boopickle.CatalogPicklers.given
import explore.model.boopickle.*
import explore.model.enums.AgsState
import explore.model.enums.Visible
import explore.model.reusability.*
import explore.model.reusability.given
import explore.model.syntax.scienceModes.*
import explore.optics.ModelOptics
import explore.syntax.ui.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ags.*
import lucuma.core.enums.PortDisposition
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.model.Observation
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import monocle.Focus
import monocle.Lens
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.document
import org.typelevel.log4cats.Logger
import queries.common.UserPreferencesQueriesGQL.*
import queries.schemas.odb.ObsQueries
import react.aladin.Fov
import react.common.ReactFnProps
import react.primereact.Checkbox
import react.primereact.Divider
import react.primereact.ProgressBar
import react.semanticui.collections.menu.*
import react.semanticui.elements.button.Button
import react.semanticui.sizes.*

import java.time.Duration
import java.time.Instant
import scala.concurrent.duration.*

case class AladinCell(
  uid:          User.Id,
  tid:          Target.Id,
  obsConf:      ObsConfiguration,
  asterism:     Asterism,
  fullScreen:   View[AladinFullScreen],
  posAngleView: Option[(View[PosAngleConstraint], View[AgsState])]
) extends ReactFnProps(AladinCell.component) {
  val setPA: Option[View[PosAngleConstraint]] = posAngleView.map(_._1)
  val agsState: Option[View[AgsState]]        = posAngleView.map(_._2)
}

trait AladinCommon:
  given Reusability[Asterism] = Reusability.by(x => (x.toSiderealTracking, x.focus.id))
  given Reusability[AgsState] = Reusability.byEq

object AladinCell extends ModelOptics with AladinCommon:
  private type Props = AladinCell

  // We want to re render only when the vizTime changes at least a month
  // We keep the candidates data pm corrected for the viz time
  // If it changes over a month we'll request the data again and recalculate
  // This way we avoid recalculating pm for example if only pos angle or
  // conditions change
  private given Reusability[Instant] = Reusability {
    Duration.between(_, _).toDays().abs < 30L
  }

  private given Reusability[Props] =
    Reusability.by(x =>
      (x.uid,
       x.tid,
       x.obsConf,
       x.agsState.map(_.reuseByValue),
       x.asterism,
       x.fullScreen.reuseByValue
      )
    )

  private val fovLens: Lens[TargetVisualOptions, Fov] =
    Lens[TargetVisualOptions, Fov](t => Fov(t.fovRA, t.fovDec))(f =>
      t => t.copy(fovRA = f.x, fovDec = f.y)
    )

  private val targetPrefs: Lens[(UserGlobalPreferences, TargetVisualOptions), TargetVisualOptions] =
    Focus[(UserGlobalPreferences, TargetVisualOptions)](_._2)

  private val userPrefs: Lens[(UserGlobalPreferences, TargetVisualOptions), UserGlobalPreferences] =
    Focus[(UserGlobalPreferences, TargetVisualOptions)](_._1)

  private def offsetViews(
    props:   Props,
    options: View[Pot[(UserGlobalPreferences, TargetVisualOptions)]]
  )(ctx:     AppContext[IO]) = {
    import ctx.given

    val offsetView =
      options.zoom(
        Pot.readyPrism.andThen(targetPrefs).andThen(TargetVisualOptions.viewOffset)
      )

    val offsetChangeInAladin = (newOffset: Offset) => {
      val ignore = options.get.fold(
        true,
        _ => true,
        o => {
          val diffP = newOffset.p.toAngle.difference(o._2.viewOffset.p.toAngle)
          val diffQ = newOffset.q.toAngle.difference(o._2.viewOffset.q.toAngle)
          // Don't save if the change is less than 1 arcse
          diffP.toMicroarcseconds < 1e6 && diffQ.toMicroarcseconds < 1e6
        }
      )

      offsetView.set(newOffset) *>
        TargetPreferences
          .updateViewOffset[IO](props.uid, props.tid, newOffset)
          .unlessA(ignore)
          .runAsync
          .rateLimit(1.seconds, 1)
          .void
    }

    // Always store the offset when centering
    val offsetOnCenter = offsetView.withOnMod {
      case Some(o) =>
        TargetPreferences
          .updateViewOffset[IO](props.uid, props.tid, o)
          .runAsync
          .void
      case _       => Callback.empty
    }
    (offsetChangeInAladin, offsetOnCenter)
  }

  def setVariable(root: Option[HTMLElement], variableName: String, value: Int): Callback =
    root
      .map(root =>
        Callback(
          root.style.setProperty(s"--aladin-image-$variableName", s"${value / 100.0}")
        )
      )
      .getOrEmpty

  private val unsafeRangeLens: Lens[TargetVisualOptions.ImageFilterRange, Double] =
    Lens[TargetVisualOptions.ImageFilterRange, Double](_.value.toDouble)(x =>
      y =>
        refineV[TargetVisualOptions.FilterRange](x.toInt).toOption
          .getOrElse(y) // Ignore invalid updates
    )

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      // target options, will be read from the user preferences
      .useStateViewWithReuse(Pot.pending[(UserGlobalPreferences, TargetVisualOptions)])
      // to get faster reusability use a serial state, rather than check every candidate
      .useSerialState(List.empty[GuideStarCandidate])
      // Analysis results
      .useSerialState(List.empty[AgsAnalysis])
      // Request data again if vizTime changes more than a month
      .useEffectWithDepsBy((p, _, __, _, _) => p.obsConf.vizTime) {
        (props, ctx, _, gs, _) => vizTime =>
          import ctx.given

          props.agsState
            .map(agsState =>
              (for {
                _          <- agsState.async.set(AgsState.LoadingCandidates)
                candidates <- CatalogClient[IO].requestSingle(
                                CatalogMessage.GSRequest(props.asterism.baseTracking, vizTime)
                              )
                _          <- candidates.map(gs.setStateAsync(_)).orEmpty
              } yield ()).guarantee(agsState.async.set(AgsState.Idle))
            )
            .orEmpty
      }
      // Reference to the root
      .useMemo(())(_ =>
        Option(document.querySelector(":root")) match
          case Some(r: HTMLElement) => r.some
          case _                    => none
      )
      // Load target preferences
      .useEffectWithDepsBy((p, _, _, _, _, _) => (p.uid, p.tid)) {
        (props, ctx, options, _, _, root) => _ =>
          import ctx.given
          TargetPreferences
            .queryWithDefault[IO](props.uid, props.tid, Constants.InitialFov)
            .flatMap { (up, tp) =>
              (options.set((up, tp).ready) *>
                setVariable(root, "saturation", tp.saturation) *>
                setVariable(root, "brightness", tp.brightness)).to[IO]
            }
      }
      // Selected GS index. Should be stored in the db
      .useStateViewWithReuse(none[Int])
      // Reset offset and gs if asterism change
      .useEffectWithDepsBy((p, _, _, _, _, _, _) => p.asterism)(
        (props, ctx, options, _, _, _, gs) =>
          _ => {
            val (_, offsetOnCenter) = offsetViews(props, options)(ctx)

            // if the coordinates change, reset ags and offset
            gs.set(none) *> offsetOnCenter.set(Offset.Zero)
          }
      )
      // Request ags calculation
      .useEffectWithDepsBy((p, _, _, candidates, _, _, _) =>
        (p.asterism.baseTracking,
         p.obsConf.posAngleConstraint,
         p.obsConf.constraints,
         p.obsConf.wavelength,
         p.obsConf.vizTime,
         p.obsConf.scienceMode,
         candidates.value
        )
      ) { (props, ctx, _, _, ags, _, selectedIndex) =>
        {
          case (tracking,
                Some(posAngle),
                Some(constraints),
                Some(wavelength),
                vizTime,
                scienceMode,
                candidates
              ) =>
            import ctx.given

            val pa = posAngle match
              case PosAngleConstraint.Fixed(a)               => NonEmptyList.of(a).some
              case PosAngleConstraint.AllowFlip(a)           => NonEmptyList.of(a, a.flip).some
              case PosAngleConstraint.ParallacticOverride(a) => None
              case _                                         => None

            (pa, tracking.at(vizTime), props.agsState).mapN { (angles, base, agsState) =>
              val positions = angles.map(pa => AgsPosition(pa, Offset.Zero))
              val fpu       = scienceMode.flatMap(_.fpuAlternative)
              val params    = AgsParams.GmosAgsParams(fpu, PortDisposition.Side)

              val sciencePositions =
                props.asterism.asList
                  .flatMap(_.toSidereal)
                  .flatMap(_.target.tracking.at(vizTime))

              val process = for
                _ <- selectedIndex.async.set(none)
                _ <- agsState.set(AgsState.Calculating).to[IO]
                _ <-
                  AgsClient[IO]
                    .requestSingle(
                      AgsMessage.Request(props.tid,
                                         constraints,
                                         wavelength,
                                         base.value,
                                         sciencePositions,
                                         positions,
                                         params,
                                         candidates
                      )
                    )
                    .map(_.map(_.selectBestPosition(positions)))
                    .flatMap { r =>
                      // Set the analysis
                      (r.map(ags.setState).getOrEmpty *>
                        // If we need to flip change the constraint
                        r
                          .map(_.headOption)
                          .collect {
                            case Some(AgsAnalysis.Usable(_, _, _, _, _, AgsPosition(angle, _))) =>
                              props.obsConf.posAngleConstraint match
                                case Some(PosAngleConstraint.AllowFlip(a)) if a =!= angle =>
                                  props.setPA
                                    .map(_.set(PosAngleConstraint.AllowFlip(angle)))
                                    .getOrEmpty
                                case _                                                    => Callback.empty
                          }
                          .orEmpty *>
                        // set the selected index
                        selectedIndex
                          .set(
                            0.some.filter(_ =>
                              r.exists(_.nonEmpty) && props.obsConf.canSelectGuideStar
                            )
                          )).to[IO]
                    }
                    .unlessA(candidates.isEmpty)
                    .handleErrorWith(t => Logger[IO].error(t)("ERROR IN AGS REQUEST"))
              yield ()
              process.guarantee(agsState.async.set(AgsState.Idle))
            }.orEmpty
          case _ => IO.unit
        }
      }
      // open settings menu
      .useState(SettingsMenuState.Closed)
      // mouse coordinates, starts on the base
      .useStateBy((props, _, _, _, _, _, _, _) => props.asterism.baseTracking.baseCoordinates)
      .renderWithReuse {
        (
          props,
          ctx,
          options,
          _,
          agsResults,
          root,
          selectedGSIndex,
          openSettings,
          mouseCoords
        ) =>
          import ctx.given

          def prefsSetter(
            candidates: Option[Visible] = None,
            overlay:    Option[Visible] = None,
            fullScreen: Option[Boolean] = None,
            saturation: Option[Int] = None,
            brightness: Option[Int] = None
          ): Callback =
            TargetPreferences
              .updateAladinPreferences[IO](
                props.uid,
                props.tid,
                agsCandidates = candidates,
                agsOverlay = overlay,
                fullScreen = fullScreen,
                saturation = saturation,
                brightness = brightness
              )
              .runAsync
              .void

          val agsCandidatesView =
            options
              .zoom(
                Pot.readyPrism.andThen(targetPrefs).andThen(TargetVisualOptions.agsCandidates)
              )
              .withOnMod(v => prefsSetter(candidates = v))

          val agsOverlayView =
            options
              .zoom(
                Pot.readyPrism.andThen(targetPrefs).andThen(TargetVisualOptions.agsOverlay)
              )
              .withOnMod(v => prefsSetter(overlay = v))

          val fovView =
            options.zoom(Pot.readyPrism.andThen(targetPrefs).andThen(fovLens))

          def cssVarView(
            varLens:        Lens[TargetVisualOptions, TargetVisualOptions.ImageFilterRange],
            variableName:   String,
            updateCallback: Int => Callback
          ) =
            options
              .zoom(
                Pot.readyPrism.andThen(targetPrefs).andThen(varLens)
              )
              .withOnMod(s =>
                s.map(s => setVariable(root, variableName, s) *> updateCallback(s)).getOrEmpty
              )

          val saturationView =
            cssVarView(TargetVisualOptions.saturation,
                       "saturation",
                       s => prefsSetter(saturation = s.some)
            )
          val brightnessView =
            cssVarView(TargetVisualOptions.brightness,
                       "brightness",
                       s => prefsSetter(brightness = s.some)
            )

          val fullScreenView =
            options
              .zoom(
                Pot.readyPrism.andThen(targetPrefs).andThen(TargetVisualOptions.fullScreen)
              )
              .withOnMod(v =>
                openSettings.setState(SettingsMenuState.Closed) *> prefsSetter(fullScreen = v)
              )

          val allowMouseZoomView =
            options
              .zoom(
                Pot.readyPrism.andThen(userPrefs).andThen(UserGlobalPreferences.aladinMouseScroll)
              )
              .withOnMod(z =>
                z.map(z => UserPreferences.storePreferences[IO](props.uid, z).runAsync).getOrEmpty
              )

          val coordinatesSetter =
            ((c: Coordinates) => mouseCoords.setState(c)).reuseAlways

          val fovSetter = (newFov: Fov) => {
            val ignore = options.get.fold(
              true,
              _ => true,
              o =>
                // Don't save if the change is less than 1 arcse
                o._2.fov.isDifferentEnough(newFov)
            )
            if (newFov.x.toMicroarcseconds === 0L) Callback.empty
            else {
              fovView.set(newFov) *>
                TargetPreferences
                  .updateAladinPreferences[IO](props.uid, props.tid, newFov.x.some, newFov.y.some)
                  .unlessA(ignore)
                  .runAsync
                  .rateLimit(1.seconds, 1)
                  .void
            }
          }

          val (offsetChangeInAladin, offsetOnCenter) = offsetViews(props, options)(ctx)

          def fullScreenSetter: Callback =
            props.fullScreen.mod(_.flip) *>
              fullScreenView.mod(!_)

          val selectedGuideStar = selectedGSIndex.get.flatMap(agsResults.value.lift)
          val usableGuideStar   = selectedGuideStar.exists(_.isUsable)

          val renderCell: ((UserGlobalPreferences, TargetVisualOptions)) => VdomNode =
            case (u: UserGlobalPreferences, t: TargetVisualOptions) =>
              AladinContainer(
                props.asterism,
                props.obsConf,
                u.aladinMouseScroll,
                t.copy(fullScreen = props.fullScreen.get.value),
                coordinatesSetter,
                fovSetter.reuseAlways,
                offsetChangeInAladin.reuseAlways,
                selectedGuideStar,
                agsResults.value
              )

          val renderToolbar: ((UserGlobalPreferences, TargetVisualOptions)) => VdomNode =
            case (_: UserGlobalPreferences, t: TargetVisualOptions) =>
              props.agsState.map(agsState =>
                AladinToolbar(Fov(t.fovRA, t.fovDec),
                              mouseCoords.value,
                              agsState.get,
                              selectedGuideStar,
                              t.agsOverlay,
                              offsetOnCenter
                )
              )

          val renderAgsOverlay: ((UserGlobalPreferences, TargetVisualOptions)) => VdomNode =
            case (u: UserGlobalPreferences, t: TargetVisualOptions) =>
              if (t.agsOverlay.visible && usableGuideStar) {
                <.div(
                  ExploreStyles.AgsOverlay,
                  AgsOverlay(
                    selectedGSIndex,
                    agsResults.value.count(_.isUsable),
                    selectedGuideStar
                  )
                )
              } else EmptyVdom

          <.div(
            ExploreStyles.TargetAladinCell,
            <.div(
              ExploreStyles.AladinContainerColumn,
              Button(size = Small, icon = true, onClick = fullScreenSetter)(
                ExploreStyles.AladinFullScreenButton,
                Icons.ExpandDiagonal.unless(props.fullScreen.get.value),
                Icons.ContractDiagonal.when(props.fullScreen.get.value)
              ),
              <.div(
                ExploreStyles.AladinToolbox,
                Button(size = Small, icon = true, onClick = openSettings.modState(_.flip))(
                  ExploreStyles.ButtonOnAladin,
                  ^.onMouseEnter --> openSettings.setState(SettingsMenuState.Open),
                  Icons.ThinSliders
                ),
                Menu(vertical = true,
                     compact = true,
                     size = Mini,
                     clazz = ExploreStyles.AladinSettingsMenu
                )(
                  ^.onMouseLeave --> openSettings.setState(SettingsMenuState.Closed),
                  MenuItem(
                    agsCandidatesView
                      .zoom(Visible.boolIso.reverse.asLens)
                      .asView
                      .map(view =>
                        CheckboxView(
                          id = "ags-candidates".refined,
                          value = view,
                          label = "Show Catalog"
                        )
                      )
                  ),
                  MenuItem(
                    agsOverlayView
                      .zoom(Visible.boolIso.reverse.asLens)
                      .asView
                      .map(view =>
                        CheckboxView(
                          id = "ags-overlay".refined,
                          value = view,
                          label = "AGS"
                        )
                      )
                  ),
                  Divider(),
                  saturationView
                    .zoom(unsafeRangeLens)
                    .asView
                    .map(view =>
                      SliderView(
                        id = "saturation".refined,
                        label = "Saturation",
                        clazz = ExploreStyles.AladinRangeControl,
                        value = view
                      )
                    ),
                  brightnessView
                    .zoom(unsafeRangeLens)
                    .asView
                    .map(view =>
                      SliderView(
                        id = "brightness".refined,
                        label = "Brightness",
                        clazz = ExploreStyles.AladinRangeControl,
                        value = view
                      )
                    ),
                  Divider(),
                  MenuItem(
                    allowMouseZoomView
                      .zoom(AladinMouseScroll.value.asLens)
                      .asView
                      .map(view =>
                        CheckboxView(
                          id = "allow-zoom".refined,
                          value = view,
                          label = "Scroll to zoom"
                        )
                      )
                  )
                ).when(openSettings.value.value)
              ),
              potRenderView[(UserGlobalPreferences, TargetVisualOptions)](renderCell)(options),
              potRenderView[(UserGlobalPreferences, TargetVisualOptions)](renderToolbar)(options),
              potRenderView[(UserGlobalPreferences, TargetVisualOptions)](renderAgsOverlay)(options)
            )
          )
      }
