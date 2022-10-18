// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import boopickle.DefaultBasic.*
import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.PotOption
import crystal.implicits.*
import crystal.react.ReuseView
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.implicits.*
import crystal.react.reuse.*
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
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import monocle.Focus
import monocle.Lens
import org.typelevel.log4cats.Logger
import queries.common.UserPreferencesQueriesGQL.*
import react.aladin.Fov
import react.common.ReactFnProps
import react.semanticui.collections.menu.*
import react.semanticui.elements.button.Button
import react.semanticui.elements.divider.Divider
import react.semanticui.modules.checkbox.Checkbox
import react.semanticui.sizes.*

import java.time.Duration
import java.time.Instant
import scala.concurrent.duration.*

case class AladinCell(
  uid:        User.Id,
  tid:        Target.Id,
  obsConf:    ObsConfiguration,
  asterism:   Asterism,
  fullScreen: View[AladinFullScreen]
) extends ReactFnProps(AladinCell.component)

object AladinCell extends ModelOptics:
  private type Props = AladinCell

  // We want to re render only when the vizTime changes at least a month
  // We keep the candidates data pm corrected for the viz time
  // If it changes over a month we'll request the data again and recalculate
  // This way we avoid recalculating pm for example if only pos angle or
  // conditions change
  private given Reusability[Instant] = Reusability {
    Duration.between(_, _).toDays().abs < 30L
  }

  private given Reusability[AgsState] = Reusability.byEq
  private given Reusability[Props]    =
    Reusability.by(x => (x.uid, x.tid, x.obsConf, x.asterism, x.fullScreen.reuseByValue))

  private val fovLens: Lens[TargetVisualOptions, Fov] =
    Lens[TargetVisualOptions, Fov](t => Fov(t.fovRA, t.fovDec))(f =>
      t => t.copy(fovRA = f.x, fovDec = f.y)
    )

  private val targetPrefs: Lens[(UserGlobalPreferences, TargetVisualOptions), TargetVisualOptions] =
    Focus[(UserGlobalPreferences, TargetVisualOptions)](_._2)

  private val userPrefs: Lens[(UserGlobalPreferences, TargetVisualOptions), UserGlobalPreferences] =
    Focus[(UserGlobalPreferences, TargetVisualOptions)](_._1)

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      // mouse coordinates, starts on the base
      .useStateBy((props, _) => props.asterism.baseTracking.baseCoordinates)
      // target options, will be read from the user preferences
      .useStateViewWithReuse(Pot.pending[(UserGlobalPreferences, TargetVisualOptions)])
      // to get faster reusability use a serial state, rather than check every candidate
      .useSerialState(List.empty[GuideStarCandidate])
      // Analysis results
      .useSerialState(List.empty[AgsAnalysis])
      // Ags state
      .useState[AgsState](AgsState.Idle)
      // Request data again if vizTime changes more than a month
      .useEffectWithDepsBy((p, _, _, _, _, _, _) => p.obsConf.vizTime) {
        (props, ctx, _, _, gs, _, agsState) => vizTime =>
          import ctx.given

          agsState.setStateAsync(AgsState.LoadingCandidates) >>
            CatalogClient[IO].requestSingle(
              CatalogMessage.GSRequest(props.asterism.baseTracking, vizTime)
            ) >>=
            (_.map(candidates =>
              agsState.setState(AgsState.Idle).to[IO] >> gs.setStateAsync(candidates)
            ).orEmpty)
      }
      .useEffectWithDepsBy((p, _, _, _, _, _, _) => (p.uid, p.tid)) {
        (props, ctx, _, options, _, _, _) => _ =>
          import ctx.given

          TargetPreferences
            .queryWithDefault[IO](props.uid, props.tid, Constants.InitialFov)
            .flatMap { (up, tp) =>
              options
                .set((up, tp).ready)
                .to[IO]
            }
      }
      // Selected GS index. Should be stored in the db
      .useStateViewWithReuse(none[Int])
      // Request ags calculation
      .useEffectWithDepsBy((p, _, _, _, candidates, _, _, _) =>
        (p.asterism.baseTracking,
         p.obsConf.posAngleConstraint,
         p.obsConf.constraints,
         p.obsConf.wavelength,
         p.obsConf.vizTime,
         p.obsConf.scienceMode,
         candidates.value
        )
      ) { (props, ctx, _, _, _, ags, agsState, selectedIndex) =>
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
              case PosAngleConstraint.Fixed(a)               => a.some
              case PosAngleConstraint.AllowFlip(a)           => a.some
              case PosAngleConstraint.ParallacticOverride(a) => a.some
              case _                                         => none

            (tracking.at(vizTime), pa).mapN { (base, pa) =>
              val basePos = AgsPosition(pa, Offset.Zero)
              val fpu     = scienceMode.flatMap(_.fpuAlternative)
              val params  = AgsParams.GmosAgsParams(fpu, PortDisposition.Side)

              val sciencePositions =
                props.asterism.asList
                  .flatMap(_.toSidereal)
                  .flatMap(_.target.tracking.at(vizTime))

              for
                _ <- selectedIndex.async.set(none)
                _ <- agsState.setStateAsync(AgsState.Calculating)
                _ <- AgsClient[IO]
                       .requestSingle(
                         AgsMessage.Request(props.tid,
                                            constraints,
                                            wavelength,
                                            base.value,
                                            sciencePositions,
                                            basePos,
                                            params,
                                            candidates
                         )
                       )
                       .flatMap(
                         _.map(r =>
                           agsState.setStateAsync(AgsState.Idle) *> ags.setStateAsync(r)
                         ).orEmpty
                       )
                       .unlessA(candidates.isEmpty)
                       .handleErrorWith(t => Logger[IO].error(t)("ERROR IN AGS REQUEST"))
              yield ()
            }.orEmpty
          case _ => IO.unit
        }
      }
      // open settings menu
      .useState(SettingsMenuState.Closed)
      // Reset the selected gs if results change
      .useEffectWithDepsBy((p, _, _, _, _, agsResults, _, _, _) => (agsResults, p.obsConf)) {
        (p, _, _, _, _, agsResults, agsState, selectedIndex, _) => _ =>
          selectedIndex
            .set(
              0.some.filter(_ => agsResults.value.nonEmpty && p.obsConf.canSelectGuideStar)
            )
            .unless_(agsState.value === AgsState.Calculating)
      }
      .renderWithReuse {
        (
          props,
          ctx,
          mouseCoords,
          options,
          _,
          agsResults,
          agsState,
          selectedGSIndex,
          openSettings
        ) =>
          import ctx.given

          val agsCandidatesView =
            options.zoom(
              Pot.readyPrism.andThen(targetPrefs).andThen(TargetVisualOptions.agsCandidates)
            )

          val agsOverlayView =
            options.zoom(
              Pot.readyPrism.andThen(targetPrefs).andThen(TargetVisualOptions.agsOverlay)
            )

          val fovView =
            options.zoom(Pot.readyPrism.andThen(targetPrefs).andThen(fovLens))

          val fullScreenView =
            options.zoom(
              Pot.readyPrism.andThen(targetPrefs).andThen(TargetVisualOptions.fullScreen)
            )

          val allowMouseZoomView =
            options.zoom(
              Pot.readyPrism.andThen(userPrefs).andThen(UserGlobalPreferences.aladinMouseScroll)
            )

          val agsCandidatesShown: Boolean = agsCandidatesView.get.map(_.visible).getOrElse(false)

          val agsOverlayShown: Boolean = agsOverlayView.get.map(_.visible).getOrElse(false)

          val allowMouseZoom: AladinMouseScroll =
            allowMouseZoomView.get.getOrElse(AladinMouseScroll.Allowed)

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
              fovView.set(newFov).unless_(ignore) *>
                TargetPreferences
                  .updateAladinPreferences[IO](props.uid, props.tid, newFov.x.some, newFov.y.some)
                  .unlessA(ignore)
                  .runAsync
                  .rateLimit(1.seconds, 1)
                  .void
            }
          }

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

          def prefsSetter(
            candidates: Visible => Visible,
            overlay:    Visible => Visible,
            fullScreen: Boolean => Boolean
          ): Callback =
            (agsCandidatesView.get, agsOverlayView.get, fullScreenView.get).mapN { (a, o, s) =>
              TargetPreferences
                .updateAladinPreferences[IO](
                  props.uid,
                  props.tid,
                  agsCandidates = candidates(a).some,
                  agsOverlay = overlay(o).some,
                  fullScreen = fullScreen(s).some
                )
                .runAsync
                .void
            }.orEmpty

          def agsOverlaySetter: Callback =
            agsOverlayView.mod(_.flip) *>
              prefsSetter(identity, _.flip, identity)

          def candidatesSetter: Callback =
            agsCandidatesView.mod(_.flip) *>
              prefsSetter(_.flip, identity, identity)

          def fullScreenSetter: Callback =
            props.fullScreen.mod(_.flip) *>
              fullScreenView.mod(!_) *>
              prefsSetter(identity, identity, !_)

          def mouseScrollSetter: Callback =
            allowMouseZoomView.mod(_.flip) *>
              UserPreferences.storePreferences[IO](props.uid, allowMouseZoom.flip).runAsync

          val aladinKey = s"${props.asterism.asList.map(_.id)}"

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
              ).withKey(aladinKey)

          val renderToolbar: ((UserGlobalPreferences, TargetVisualOptions)) => VdomNode =
            case (_: UserGlobalPreferences, t: TargetVisualOptions) =>
              AladinToolbar(Fov(t.fovRA, t.fovDec),
                            mouseCoords.value,
                            agsState.value,
                            selectedGuideStar,
                            t.agsOverlay,
                            offsetOnCenter
              ): VdomNode

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
                    Checkbox(
                      label = "Show Catalog",
                      checked = agsCandidatesShown,
                      onChange = (_: Boolean) =>
                        openSettings.setState(SettingsMenuState.Closed) *> candidatesSetter
                    )
                  ),
                  MenuItem(
                    Checkbox(
                      label = "AGS",
                      checked = agsOverlayShown,
                      onChange = (_: Boolean) =>
                        openSettings.setState(SettingsMenuState.Closed) *> agsOverlaySetter
                    )
                  ),
                  Divider(fitted = true),
                  MenuItem(
                    Checkbox(
                      label = "Scroll to zoom",
                      checked = allowMouseZoom.value,
                      onChange = (_: Boolean) =>
                        openSettings.setState(SettingsMenuState.Closed) *> mouseScrollSetter
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
