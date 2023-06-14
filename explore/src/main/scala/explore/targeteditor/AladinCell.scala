// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import explore.aladin.AladinFullScreenControl
import explore.common.UserPreferencesQueries.*
import explore.components.ui.ExploreStyles
import explore.events.*
import explore.model.WorkerClients.*
import explore.model.*
import explore.model.boopickle.Boopickle.*
import explore.model.boopickle.CatalogPicklers.given
import explore.model.boopickle.*
import explore.model.enums.AgsState
import explore.model.enums.Visible
import explore.model.reusability.given
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
import lucuma.core.util.NewType
import lucuma.refined.*
import lucuma.schemas.model.TargetWithId
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
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
import react.common.*
import react.primereact.Button
import react.primereact.Checkbox
import react.primereact.MenuItem
import react.primereact.PopupMenu
import react.primereact.PopupMenuRef
import react.primereact.ProgressBar
import react.primereact.hooks.all.*

import java.time.Duration
import java.time.Instant
import scala.concurrent.duration.*

case class AladinCell(
  uid:        User.Id,
  tid:        Target.Id,
  vizTime:    Instant,
  obsConf:    Option[ObsConfiguration],
  asterism:   Asterism,
  fullScreen: View[AladinFullScreen]
) extends ReactFnProps(AladinCell.component):
  val anglesToTest: Option[NonEmptyList[Angle]] =
    for {
      conf          <- obsConf
      configuration <- conf.configuration
      paConstraint  <- conf.posAngleConstraint
      angles        <-
        paConstraint.anglesToTestAt(configuration.siteFor, asterism.baseTracking, vizTime)
    } yield
    // We sort the angles or we could end up in a loop where the angles are tested back and forth
    // This is rare but can happen if each angle finds an equivalent guide star
    angles.sorted(using Angle.AngleOrder)

  val positions: Option[NonEmptyList[AgsPosition]] =
    val offsets: NonEmptyList[Offset] = obsConf.flatMap(_.scienceOffsets) match
      case Some(offsets) => offsets.prepend(Offset.Zero)
      case None          => NonEmptyList.of(Offset.Zero)

    anglesToTest.map { anglesToTest =>
      for {
        pa  <- anglesToTest
        off <- offsets
      } yield AgsPosition(pa, off)
    }

  def canRunAGS: Boolean = obsConf.exists(o => o.constraints.isDefined && o.wavelength.isDefined)

trait AladinCommon:
  given Reusability[Asterism] = Reusability.by(x => (x.toSiderealTracking, x.focus.id))
  given Reusability[AgsState] = Reusability.byEq

object AladinCell extends ModelOptics with AladinCommon:
  private object ManualAgsOverride extends NewType[Boolean]
  private type ManualAgsOverride = ManualAgsOverride.Type

  private type Props = AladinCell
  private given Reusability[View[ManualAgsOverride]] = Reusability.by(_.get)
  private given Reusability[PopupMenuRef]            =
    Reusability.by(ref => Option(ref.ref.raw.current).isDefined)

  // We want to re render only when the vizTime changes at least a month
  // We keep the candidates data pm corrected for the viz time
  // If it changes over a month we'll request the data again and recalculate
  // This way we avoid recalculating pm for example if only pos angle or
  // conditions change
  private given Reusability[Instant] = Reusability[Instant] {
    Duration.between(_, _).toDays().abs < 30L
  }

  private given Reusability[Props] =
    Reusability.by(x => (x.uid, x.tid, x.obsConf, x.asterism, x.fullScreen.reuseByValue))

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
  )(ctx: AppContext[IO]) = {
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

  private def flipAngle(
    props:          Props,
    manualOverride: View[ManualAgsOverride]
  ): Option[AgsAnalysis] => Callback =
    case Some(AgsAnalysis.Usable(_, _, _, _, pos)) =>
      val angle = pos.head._1.posAngle
      props.obsConf.flatMap(_.posAngleConstraint) match
        case Some(PosAngleConstraint.AllowFlip(a)) if a =!= angle =>
          props.obsConf
            .flatMap(_.posAngleConstraintView)
            .map(_.set(PosAngleConstraint.AllowFlip(a.flip)))
            .getOrEmpty *> manualOverride.set(ManualAgsOverride(true))
        case _                                                    => Callback.empty
    case _                                         => Callback.empty

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
      .useEffectWithDepsBy((p, _, _, _, _) => (p.vizTime, p.asterism.baseTracking)) {
        (props, ctx, _, gs, _) => (vizTime, baseTracking) =>
          import ctx.given

          (for {
            _          <- props.obsConf
                            .flatMap(_.agsState)
                            .foldMap(_.async.set(AgsState.LoadingCandidates))
            candidates <- CatalogClient[IO].requestSingle(
                            CatalogMessage.GSRequest(baseTracking, vizTime)
                          )
            _          <- candidates.map(gs.setStateAsync(_)).orEmpty
          } yield ()).guarantee(
            props.obsConf.flatMap(_.agsState).foldMap(_.async.set(AgsState.Idle))
          )
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
      // mouse coordinates, starts on the base
      .useStateBy((props, _, _, _, _, _, _) => props.asterism.baseTracking.baseCoordinates)
      // Reset offset and gs if asterism change
      .useEffectWithDepsBy((p, _, _, _, _, _, _, _) => p.asterism)(
        (props, ctx, options, _, _, _, gs, mouseCoords) =>
          _ => {
            val (_, offsetOnCenter) = offsetViews(props, options)(ctx)

            // if the coordinates change, reset ags, offset and mouse coordinates
            gs.set(none) *> offsetOnCenter.set(Offset.Zero) *> mouseCoords.setState(
              props.asterism.baseTracking.baseCoordinates
            )
          }
      )
      .useStateView(ManualAgsOverride(false))
      // Reset selection if pos angle changes except for manual selection changes
      .useEffectWithDepsBy((p, _, _, _, _, _, _, _, _) => p.obsConf.flatMap(_.posAngleConstraint))(
        (_, _, _, _, _, _, selectedIndex, _, agsOverride) =>
          _ => selectedIndex.set(none).unless_(agsOverride.get.value)
      )
      // Request ags calculation
      .useEffectWithDepsBy((p, _, _, candidates, _, _, _, _, _) =>
        (p.asterism.baseTracking,
         p.asterism.focus.id,
         p.positions,
         p.obsConf.flatMap(_.constraints),
         p.obsConf.flatMap(_.wavelength),
         p.vizTime,
         p.obsConf.flatMap(_.configuration),
         candidates.value
        )
      ) { (props, ctx, _, _, ags, _, selectedIndex, _, agsOverride) =>
        {
          case (tracking,
                _,
                positions,
                Some(constraints),
                Some(wavelength),
                vizTime,
                observingMode,
                candidates
              ) =>
            import ctx.given

            (selectedIndex.set(none) *>
              props.obsConf.flatMap(_.selectedGS).map(_.set(none)).orEmpty)
              .to[IO]
              .whenA(positions.isEmpty) *>
              (positions, tracking.at(vizTime), props.obsConf.flatMap(_.agsState)).mapN {
                (positions, base, agsState) =>

                  val fpu    = observingMode.flatMap(_.fpuAlternative)
                  val params = AgsParams.GmosAgsParams(fpu, PortDisposition.Side)

                  val sciencePositions =
                    props.asterism.asList
                      .flatMap(_.toSidereal)
                      .flatMap(_.target.tracking.at(vizTime))

                  val process = for
                    _ <- agsState.set(AgsState.Calculating).to[IO]
                    _ <-
                      AgsClient[IO]
                        .requestSingle(
                          AgsMessage.AgsRequest(props.tid,
                                                constraints,
                                                wavelength,
                                                base.value,
                                                sciencePositions,
                                                positions,
                                                params,
                                                candidates
                          )
                        )
                        .map(_.map(_.sortPositions(positions)))
                        .flatMap { r =>
                          // Set the analysis
                          (r.map(ags.setState).getOrEmpty *>
                            // If we need to flip change the constraint
                            r
                              .map(_.headOption)
                              .map(flipAngle(props, agsOverride))
                              .orEmpty
                              .unlessA(agsOverride.get.value) *>
                            // set the selected index to the first entry
                            {
                              val index      = 0.some.filter(_ => r.exists(_.nonEmpty))
                              val selectedGS = index.flatMap(i => r.flatMap(_.lift(i)))
                              (selectedIndex.set(index) *>
                                props.obsConf
                                  .flatMap(_.selectedGS)
                                  .map(_.set(selectedGS))
                                  .getOrEmpty).unlessA(agsOverride.get.value)
                            }).to[IO]
                        }
                        .unlessA(candidates.isEmpty)
                        .handleErrorWith(t => Logger[IO].error(t)("ERROR IN AGS REQUEST"))
                  yield ()
                  process.guarantee(
                    agsOverride.async.set(ManualAgsOverride(false)) *> agsState.async.set(
                      AgsState.Idle
                    )
                  )
              }.orEmpty
          case _ => IO.unit
        }
      }
      .usePopupMenuRef
      .renderWithReuse {
        (
          props,
          ctx,
          options,
          _,
          agsResults,
          root,
          selectedGSIndexView,
          mouseCoords,
          agsManualOverride,
          menuRef
        ) =>
          import ctx.given

          // If the selected GS changes do a flip when necessary
          val selectedGSIndex = selectedGSIndexView.withOnMod(idx =>
            idx
              .map(agsResults.value.lift)
              .map(a =>
                flipAngle(props, agsManualOverride)(a) *> props.obsConf
                  .flatMap(_.selectedGS)
                  .map(_.set(a))
                  .getOrEmpty
              )
              .getOrEmpty
          )

          def prefsSetter(
            saturation: Option[Int] = None,
            brightness: Option[Int] = None
          ): Callback =
            TargetPreferences
              .updateAladinPreferences[IO](
                props.uid,
                props.tid,
                saturation = saturation,
                brightness = brightness
              )
              .runAsync
              .void

          def userSetter(
            showCatalog:        Option[Visible] = None,
            agsOverlay:         Option[Visible] = None,
            fullScreen:         Option[AladinFullScreen] = None,
            scienceOffsets:     Option[Visible] = None,
            acquisitionOffsets: Option[Visible] = None
          ): Callback =
            UserPreferences
              .storePreferences[IO](
                props.uid,
                showCatalog = showCatalog,
                agsOverlay = agsOverlay,
                scienceOffsets = scienceOffsets,
                acquisitionOffsets = acquisitionOffsets,
                fullScreen = fullScreen
              )
              .runAsync
              .void

          def visiblePropView(
            get:   Lens[UserGlobalPreferences, Visible],
            onMod: Option[Visible] => Callback
          ) =
            options
              .zoom(Pot.readyPrism.andThen(userPrefs).andThen(get))
              .withOnMod(onMod)
              .zoom(Visible.value.asLens)

          val agsCandidatesView =
            visiblePropView(UserGlobalPreferences.aladinShowCatalog,
                            v => userSetter(showCatalog = v)
            )

          val agsOverlayView =
            visiblePropView(UserGlobalPreferences.aladinAgsOverlay, v => userSetter(agsOverlay = v))

          val scienceOffsetsView =
            visiblePropView(UserGlobalPreferences.aladinScienceOffsets,
                            v => userSetter(scienceOffsets = v)
            )

          val acquisitionOffsetsView =
            visiblePropView(UserGlobalPreferences.aladinAcquisitionOffsets,
                            v => userSetter(acquisitionOffsets = v)
            )

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
                Pot.readyPrism.andThen(userPrefs).andThen(UserGlobalPreferences.fullScreen)
              )
              .withOnMod(v =>
                v.map(v => props.fullScreen.set(v)).getOrEmpty *> userSetter(fullScreen = v)
              )

          val allowMouseZoomView =
            options
              .zoom(
                Pot.readyPrism.andThen(userPrefs).andThen(UserGlobalPreferences.aladinMouseScroll)
              )
              .withOnMod(z =>
                z.map(z => UserPreferences.storePreferences[IO](props.uid, z.some).runAsync)
                  .getOrEmpty
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

          val selectedGuideStar = selectedGSIndex.get.flatMap(agsResults.value.lift)

          val renderCell: ((UserGlobalPreferences, TargetVisualOptions)) => VdomNode =
            case (u: UserGlobalPreferences, t: TargetVisualOptions) =>
              AladinContainer(
                props.asterism,
                props.vizTime,
                props.obsConf,
                u,
                t,
                coordinatesSetter,
                fovSetter.reuseAlways,
                offsetChangeInAladin.reuseAlways,
                selectedGuideStar,
                agsResults.value
              )

          val renderToolbar: ((UserGlobalPreferences, TargetVisualOptions)) => VdomNode =
            case (u: UserGlobalPreferences, t: TargetVisualOptions) =>
              val agsState = props.obsConf
                .flatMap(_.agsState.map(_.get))
                .getOrElse(AgsState.Idle)
              AladinToolbar(Fov(t.fovRA, t.fovDec),
                            mouseCoords.value,
                            agsState,
                            selectedGuideStar,
                            u.aladinAgsOverlay,
                            offsetOnCenter
              )

          val renderAgsOverlay: ((UserGlobalPreferences, TargetVisualOptions)) => VdomNode =
            case (u: UserGlobalPreferences, t: TargetVisualOptions) =>
              if (u.aladinAgsOverlay.isVisible && props.canRunAGS) {
                props.obsConf
                  .flatMap(_.agsState)
                  .map(agsState =>
                    <.div(
                      ExploreStyles.AgsOverlay,
                      AgsOverlay(
                        selectedGSIndex,
                        agsResults.value.count(_.isUsable),
                        selectedGuideStar,
                        agsState.get
                      )
                    )
                  )
              } else EmptyVdom

          val menuItems = List(
            MenuItem.Custom(
              agsCandidatesView.asView
                .map(view =>
                  CheckboxView(
                    id = "ags-candidates".refined,
                    value = view,
                    label = "Show Catalog"
                  )
                )
            ),
            MenuItem.Custom(
              agsOverlayView.asView
                .map(view =>
                  CheckboxView(
                    id = "ags-overlay".refined,
                    value = view,
                    label = "AGS"
                  )
                )
            ),
            MenuItem.Custom(
              scienceOffsetsView.asView
                .map(view =>
                  CheckboxView(
                    id = "science-offsets".refined,
                    value = view,
                    label = "Sci. Offsets"
                  )
                )
            ),
            MenuItem.Custom(
              acquisitionOffsetsView.asView
                .map(view =>
                  CheckboxView(
                    id = "acq-offsets".refined,
                    value = view,
                    label = "Acq. Offsets"
                  )
                )
            ),
            MenuItem.Separator,
            MenuItem.Custom(
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
                )
            ),
            MenuItem.Custom(
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
                )
            ),
            MenuItem.Separator,
            MenuItem.Custom(
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
          )

          <.div(
            ExploreStyles.TargetAladinCell,
            <.div(
              ExploreStyles.AladinContainerColumn,
              AladinFullScreenControl(fullScreenView.value),
              <.div(
                ExploreStyles.AladinToolbox,
                Button(onClickE = menuRef.toggle).withMods(
                  ExploreStyles.ButtonOnAladin,
                  Icons.ThinSliders
                )
              ),
              options.renderPotView(renderCell),
              options.renderPotView(renderToolbar),
              options.renderPotView(renderAgsOverlay)
            ),
            PopupMenu(model = menuItems, clazz = ExploreStyles.AladinSettingsMenu)
              .withRef(menuRef.ref)
          )
      }
