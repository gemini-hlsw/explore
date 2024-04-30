// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import boopickle.DefaultBasic.*
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.*
import crystal.Pot
import crystal.react.*
import crystal.react.hooks.*
import crystal.react.reuse.*
import eu.timepit.refined.*
import eu.timepit.refined.auto.*
import explore.Icons
import explore.aladin.AladinFullScreenControl
import explore.common.UserPreferencesQueries
import explore.common.UserPreferencesQueries.AsterismPreferences
import explore.common.UserPreferencesQueries.GlobalUserPreferences
import explore.components.ui.ExploreStyles
import explore.events.*
import explore.model.*
import explore.model.AppContext
import explore.model.WorkerClients.*
import explore.model.boopickle.*
import explore.model.boopickle.CatalogPicklers.given
import explore.model.enums.AgsState
import explore.model.enums.Visible
import explore.model.reusability.given
import explore.optics.ModelOptics
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ags.*
import lucuma.core.enums.PortDisposition
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.NewType
import lucuma.react.aladin.Fov
import lucuma.react.common.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.hooks.all.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import monocle.Lens
import org.typelevel.log4cats.Logger
import queries.schemas.UserPreferencesDB

import java.time.Duration
import java.time.Instant
import scala.concurrent.duration.*

case class AladinCell(
  uid:               User.Id,
  asterism:          Asterism,
  vizTime:           Instant,
  obsConf:           Option[ObsConfiguration],
  fullScreen:        View[AladinFullScreen],
  globalPreferences: View[GlobalPreferences]
) extends ReactFnProps(AladinCell.component):
  val anglesToTest: Option[NonEmptyList[Angle]] =
    for {
      conf          <- obsConf
      configuration <- conf.configuration
      paConstraint  <- conf.posAngleConstraint
      angles        <-
        paConstraint.anglesToTestAt(configuration.siteFor,
                                    asterism.baseTracking,
                                    vizTime,
                                    Duration.ofHours(1)
        )
      // We sort the angles or we could end up in a loop where the angles are tested back and forth
      // This is rare but can happen if each angle finds an equivalent guide star
    } yield angles.sorted(using Angle.AngleOrder)

  val positions: Option[NonEmptyList[AgsPosition]] =
    val offsets: NonEmptyList[Offset] = obsConf
      .flatMap { o =>
        (o.scienceOffsets, o.acquisitionOffsets) match
          case (Some(sci), Some(acq)) => (sci ++ acq.toList).toNes.toNonEmptyList.some
          case (Some(sci), None)      => sci.some
          case (None, Some(acq))      => acq.some
          case (None, None)           => none
      }
      .getOrElse(NonEmptyList.of(Offset.Zero))

    anglesToTest.map: anglesToTest =>
      for {
        pa  <- anglesToTest
        off <- offsets
      } yield AgsPosition(pa, off)

  def canRunAGS: Boolean = obsConf.exists(o =>
    o.constraints.isDefined && o.configuration.isDefined && o.wavelength.isDefined && positions.isDefined
  )

  def sciencePositionsAt(vizTime: Instant): List[Coordinates] =
    asterism.asList
      .flatMap(_.toSidereal)
      .flatMap(_.target.tracking.at(vizTime))
end AladinCell

trait AladinCommon:
  given Reusability[Asterism] = Reusability.by(x => (x.toSiderealTracking, x.focus.id))
  given Reusability[AgsState] = Reusability.byEq

  def userPrefsSetter(
    uid:                User.Id,
    showCatalog:        Option[Visible] = None,
    agsOverlay:         Option[Visible] = None,
    fullScreen:         Option[AladinFullScreen] = None,
    scienceOffsets:     Option[Visible] = None,
    acquisitionOffsets: Option[Visible] = None
  )(using Logger[IO], FetchClient[IO, UserPreferencesDB]): Callback =
    GlobalUserPreferences
      .storeAladinPreferences[IO](
        uid,
        showCatalog = showCatalog,
        agsOverlay = agsOverlay,
        scienceOffsets = scienceOffsets,
        acquisitionOffsets = acquisitionOffsets,
        fullScreen = fullScreen
      )
      .runAsync
      .void

object AladinCell extends ModelOptics with AladinCommon:
  private object ManualAgsOverride extends NewType[Boolean]
  private type ManualAgsOverride = ManualAgsOverride.Type

  private type Props = AladinCell

  // We want to re render only when the vizTime changes at least a month
  // We keep the candidates data pm corrected for the viz time
  // If it changes over a month we'll request the data again and recalculate
  // This way we avoid recalculating pm for example if only pos angle or
  // conditions change
  private given Reusability[Instant] = Reusability[Instant] {
    Duration.between(_, _).toDays().abs < 30L
  }

  private val fovLens: Lens[AsterismVisualOptions, Fov] =
    Lens[AsterismVisualOptions, Fov](t => Fov(t.fovRA, t.fovDec))(f =>
      t => t.copy(fovRA = f.x, fovDec = f.y)
    )

  private def offsetViews(
    props:   Props,
    options: View[Pot[AsterismVisualOptions]]
  )(ctx: AppContext[IO]) = {
    import ctx.given

    val offsetView =
      options.zoom(
        Pot.readyPrism.andThen(AsterismVisualOptions.viewOffset)
      )

    val offsetChangeInAladin = (newOffset: Offset) => {
      val ignore = options.get.fold(
        true,
        _ => true,
        o => {
          val diffP = newOffset.p.toAngle.difference(o.viewOffset.p.toAngle)
          val diffQ = newOffset.q.toAngle.difference(o.viewOffset.q.toAngle)
          // Don't save if the change is less than 1 arcse
          diffP.toMicroarcseconds < 1e6 && diffQ.toMicroarcseconds < 1e6
        }
      )

      offsetView.set(newOffset) *>
        AsterismPreferences
          .updateAladinPreferences[IO](options.get.toOption.flatMap(_.id),
                                       props.uid,
                                       props.asterism.ids,
                                       offset = newOffset.some
          )
          .unlessA(ignore)
          .runAsync
          .rateLimit(1.seconds, 1)
          .void
    }

    // Always store the offset when centering
    val offsetOnCenter = offsetView.withOnMod {
      case o @ Some(_) =>
        AsterismPreferences
          .updateAladinPreferences[IO](options.get.toOption.flatMap(_.id),
                                       props.uid,
                                       props.asterism.ids,
                                       offset = o
          )
          .void
          .runAsync
      case _           => Callback.empty
    }
    (offsetChangeInAladin, offsetOnCenter)
  }

  private def flipAngle(
    props:          Props,
    manualOverride: View[ManualAgsOverride]
  ): Option[AgsAnalysis] => Callback =
    case Some(AgsAnalysis.Usable(_, _, _, _, pos)) =>
      val angle = pos.head._1
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
      .useStateView(Pot.pending[AsterismVisualOptions])
      // to get faster reusability use a serial state, rather than check every candidate
      .useSerialState(none[List[GuideStarCandidate]])
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
            candidates <- CatalogClient[IO]
                            .requestSingle(
                              CatalogMessage.GSRequest(baseTracking, vizTime)
                            )
            _          <- gs.setStateAsync(candidates)
          } yield ()).guarantee(
            props.obsConf.flatMap(_.agsState).foldMap(_.async.set(AgsState.Idle))
          )
      }
      // Reference to root
      .useMemo(())(_ => domRoot)
      // Load target preferences
      .useEffectWithDepsBy((p, _, _, _, _, _) => (p.uid, p.asterism.ids)) {
        (props, ctx, options, _, _, root) => _ =>
          import ctx.given

          AsterismPreferences
            .queryWithDefault[IO](props.uid, props.asterism.ids, Constants.InitialFov)
            .flatMap { tp =>
              (options.set(tp.ready) *>
                setVariable(root, "saturation", tp.saturation) *>
                setVariable(root, "brightness", tp.brightness)).toAsync
            }
      }
      // Selected GS index. Should be stored in the db
      .useStateView(none[Int])
      // mouse coordinates, starts on the base
      .useStateBy((props, _, _, _, _, _, _) => props.asterism.baseTracking.baseCoordinates)
      // Reset offset and gs if asterism change
      .useEffectWithDepsBy((p, _, _, _, _, _, _, _) => p.asterism)(
        (props, ctx, options, _, _, _, gs, mouseCoords) =>
          _ =>
            val (_, offsetOnCenter) = offsetViews(props, options)(ctx)

            // if the coordinates change, reset ags, offset and mouse coordinates
            for {
              _ <- gs.set(none)
              _ <- offsetOnCenter.set(Offset.Zero)
              _ <- mouseCoords.setState(props.asterism.baseTracking.baseCoordinates)
            } yield ()
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
         p.obsConf.flatMap(_.posAngleConstraint),
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
                _,
                Some(constraints),
                Some(wavelength),
                vizTime,
                observingMode,
                candidates
              ) =>
            import ctx.given

            val runAgs = (positions,
                          tracking.at(vizTime),
                          props.obsConf.flatMap(_.agsState),
                          candidates.value
            ).mapN { (positions, base, agsState, candidates) =>

              val fpu    = observingMode.flatMap(_.fpuAlternative)
              val params = AgsParams.GmosAgsParams(fpu, PortDisposition.Side)

              val request = AgsMessage.AgsRequest(props.asterism.focus.id,
                                                  constraints,
                                                  wavelength,
                                                  base.value,
                                                  props.sciencePositionsAt(vizTime),
                                                  positions,
                                                  params,
                                                  candidates
              )

              def processResults(r: Option[List[AgsAnalysis]]): IO[Unit] =
                (for {
                  // Set the analysis
                  _ <- r.map(ags.setState).getOrEmpty
                  // If we need to flip change the constraint
                  _ <- r
                         .map(_.headOption)
                         .map(flipAngle(props, agsOverride))
                         .orEmpty
                         .unlessA(agsOverride.get.value)
                  // set the selected index to the first entry
                  _ <- {
                    val index      = 0.some.filter(_ => r.exists(_.nonEmpty))
                    val selectedGS = index.flatMap(i => r.flatMap(_.lift(i)))
                    (selectedIndex.set(index) *>
                      props.obsConf
                        .flatMap(_.selectedGS)
                        .map(_.set(selectedGS))
                        .getOrEmpty).unlessA(agsOverride.get.value)
                  }
                } yield ()).toAsync

              val process: IO[Unit] =
                for {
                  _ <- agsState.set(AgsState.Calculating).toAsync
                  _ <-
                    AgsClient[IO]
                      .requestSingle(request)
                      .flatMap(processResults(_))
                      .unlessA(candidates.isEmpty)
                      .handleErrorWith(t => Logger[IO].error(t)("ERROR IN AGS REQUEST"))
                } yield ()

              process.guarantee(
                agsOverride.async.set(ManualAgsOverride(false)) *> agsState.async.set(
                  AgsState.Idle
                )
              )
            }.orEmpty

            (selectedIndex.set(none) *>
              props.obsConf.flatMap(_.selectedGS).map(_.set(none)).orEmpty).toAsync
              .whenA(positions.isEmpty) *> runAgs
          case _ => IO.unit
        }
      }
      .usePopupMenuRef
      .render {
        (
          props,
          ctx,
          options,
          candidates,
          agsResults,
          _,
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

          val fovView =
            options.zoom(Pot.readyPrism.andThen(fovLens))

          val fullScreenView =
            props.globalPreferences
              .zoom(GlobalPreferences.fullScreen)
              .withOnMod(v =>
                props.fullScreen.set(v) *> userPrefsSetter(props.uid, fullScreen = v.some)
              )

          val coordinatesSetter =
            ((c: Coordinates) => mouseCoords.setState(c)).reuseAlways

          val fovSetter = (newFov: Fov) => {
            val ignore = options.get.fold(
              true,
              _ => true,
              o =>
                // Don't save if the change is less than 1 arcse
                o.fov.isDifferentEnough(newFov)
            )
            if (newFov.x.toMicroarcseconds === 0L) Callback.empty
            else {
              fovView.set(newFov) *>
                AsterismPreferences
                  .updateAladinPreferences[IO](options.get.toOption.flatMap(_.id),
                                               props.uid,
                                               props.asterism.ids,
                                               newFov.x.some,
                                               newFov.y.some
                  )
                  .unlessA(ignore)
                  .runAsync
                  .rateLimit(1.seconds, 1)
                  .void
            }
          }

          val (offsetChangeInAladin, offsetOnCenter) = offsetViews(props, options)(ctx)

          val selectedGuideStar = selectedGSIndex.get.flatMap(agsResults.value.lift)

          val renderCell: AsterismVisualOptions => VdomNode =
            (t: AsterismVisualOptions) =>
              AladinContainer(
                props.asterism,
                props.vizTime,
                props.obsConf,
                props.globalPreferences.get,
                t,
                coordinatesSetter,
                fovSetter,
                offsetChangeInAladin.reuseAlways,
                selectedGuideStar,
                agsResults.value
              )

          val renderToolbar: (AsterismVisualOptions) => VdomNode =
            (t: AsterismVisualOptions) =>
              val agsState = props.obsConf
                .flatMap(_.agsState.map(_.get))
                .getOrElse(AgsState.Idle)
              AladinToolbar(Fov(t.fovRA, t.fovDec),
                            mouseCoords.value,
                            agsState,
                            selectedGuideStar,
                            props.globalPreferences.get.agsOverlay,
                            offsetOnCenter
              )

          val renderAgsOverlay: AsterismVisualOptions => VdomNode =
            (t: AsterismVisualOptions) =>
              if (props.globalPreferences.get.agsOverlay.isVisible) {
                props.obsConf
                  .flatMap(_.agsState)
                  .map(agsState =>
                    <.div(
                      ExploreStyles.AgsOverlay,
                      AgsOverlay(
                        selectedGSIndex,
                        agsResults.value.count(_.isUsable),
                        selectedGuideStar,
                        agsState.get,
                        props.canRunAGS,
                        candidates.value.isDefined
                      )
                    )
                  )
              } else EmptyVdom

          <.div(
            ExploreStyles.TargetAladinCell,
            <.div(
              ExploreStyles.AladinContainerColumn,
              AladinFullScreenControl(fullScreenView),
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
            options
              .zoom(Pot.readyPrism[AsterismVisualOptions])
              .mapValue(options =>
                AladinPreferencesMenu(props.uid,
                                      props.asterism.ids,
                                      props.globalPreferences,
                                      options,
                                      menuRef
                )
              )
          )
      }
