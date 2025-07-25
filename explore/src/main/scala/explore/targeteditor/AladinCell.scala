// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import boopickle.DefaultBasic.*
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import crystal.react.reuse.*
import eu.timepit.refined.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.UserPreferencesQueries.AsterismPreferences
import explore.common.UserPreferencesQueries.GlobalUserPreferences
import explore.components.ui.ExploreStyles
import explore.events.*
import explore.model.*
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
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.PortDisposition
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.model.User
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.react.common.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.hooks.all.*
import lucuma.schemas.model.BasicConfiguration
import lucuma.ui.aladin.AladinFullScreen as UIFullScreen
import lucuma.ui.aladin.AladinFullScreenControl
import lucuma.ui.aladin.Fov
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import monocle.Iso
import monocle.Lens
import org.typelevel.log4cats.Logger
import queries.schemas.UserPreferencesDB

import java.time.Instant
import scala.concurrent.duration.*

case class AladinCell(
  uid:                User.Id,
  obsId:              Option[Observation.Id],
  asterism:           Asterism,
  obsTime:            Instant,
  obsConf:            Option[ObsConfiguration],
  fullScreen:         View[AladinFullScreen],
  userPreferences:    View[UserPreferences],
  guideStarSelection: View[GuideStarSelection]
) extends ReactFnProps(AladinCell.component):
  val needsAGS: Boolean =
    obsConf.exists(_.needGuideStar)

  val siderealDiscretizedObsTime: SiderealDiscretizedObsTime =
    SiderealDiscretizedObsTime(obsTime, obsConf.flatMap(_.posAngleConstraint))

  val anglesToTest: Option[NonEmptyList[Angle]] =
    for
      conf         <- obsConf
      paConstraint <- conf.posAngleConstraint
      angles       <-
        // For visual mode we want to default to PA 0 if needed e.g. average parallactic not available
        paConstraint
          .anglesToTestAt(obsConf.flatMap(_.averagePA).map(_.averagePA))
          .orElse(NonEmptyList.one(Angle.Angle0).some)
    // We sort the angles or we could end up in a loop where the angles are tested back and forth
    // This is rare but can happen if each angle finds an equivalent guide star
    yield angles.sorted(using Angle.AngleOrder)

  val positions: Option[NonEmptyList[AgsPosition]] =
    val offsets: NonEmptyList[Offset] = obsConf
      .flatMap: o =>
        (o.scienceOffsets, o.acquisitionOffsets) match
          case (Some(sci), Some(acq)) => (sci ++ acq.toList).toNes.toNonEmptyList.some
          case (Some(sci), None)      => sci.some
          case (None, Some(acq))      => acq.some
          case (None, None)           => none
      .getOrElse(NonEmptyList.of(Offset.Zero))

    anglesToTest.map: anglesToTest =>
      for {
        pa  <- anglesToTest
        off <- offsets
      } yield AgsPosition(pa, off)

  def durationAvailable: Boolean =
    obsConf.flatMap(_.obsDuration).isDefined

  def modeSelected: Boolean =
    obsConf.exists(_.configuration.isDefined)

  def selectedGSName: Option[NonEmptyString] =
    obsConf.flatMap(_.remoteGSName)

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
  import GuideStarSelection.*

  private type Props = AladinCell

  // only compare candidates by id
  private given Reusability[GuideStarCandidate] = Reusability.by(_.id)

  private val fovLens: Lens[AsterismVisualOptions, Fov] =
    Lens[AsterismVisualOptions, Fov](t => Fov(t.fovRA, t.fovDec)): f =>
      t => t.copy(fovRA = f.x, fovDec = f.y)

  val fullScreenIso: Iso[AladinFullScreen, UIFullScreen] =
    Iso[AladinFullScreen, UIFullScreen](x => UIFullScreen(x.value))(x => AladinFullScreen(x.value))

  private def offsetViews(
    props:   Props,
    options: View[Pot[AsterismVisualOptions]]
  )(ctx: AppContext[IO]) = {
    import ctx.given

    val offsetView: ViewOpt[Offset] =
      options.zoom:
        Pot.readyPrism.andThen(AsterismVisualOptions.viewOffset)

    val offsetChangeInAladin = (newOffset: Offset) => {
      val ignore = options.get.fold(
        true,
        _ => true,
        o =>
          val diffP = newOffset.p.toAngle.difference(o.viewOffset.p.toAngle)
          val diffQ = newOffset.q.toAngle.difference(o.viewOffset.q.toAngle)
          // Don't save if the change is less than 1 arcse
          diffP.toMicroarcseconds < 1e6 && diffQ.toMicroarcseconds < 1e6
      )

      offsetView.set(newOffset) *>
        AsterismPreferences
          .updateAladinPreferences[IO](
            options.get.toOption.flatMap(_.id),
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
    val offsetOnCenter = offsetView.withOnMod:
      case o @ Some(_) =>
        AsterismPreferences
          .updateAladinPreferences[IO](
            options.get.toOption.flatMap(_.id),
            props.uid,
            props.asterism.ids,
            offset = o
          )
          .void
          .runAsync
      case _           => Callback.empty

    (offsetChangeInAladin, offsetOnCenter)
  }

  private val component = ScalaFnComponent[Props]: props =>
    for {
      ctx               <- useContext(AppContext.ctx)
      // Request guide star candidates if obsTime changes more than a month or the base moves
      candidates        <- useEffectResultWithDeps(
                             (props.siderealDiscretizedObsTime,
                              props.asterism.baseTracking,
                              props.obsConf.flatMap(_.obsModeType)
                             )
                           ): (siderealDiscretizedObsTime, baseTracking, obsModeType) =>
                             import ctx.given

                             if (props.needsAGS && obsModeType.isDefined)
                               (for
                                 _          <- props.obsConf
                                                 .flatMap(_.agsState)
                                                 .foldMap(_.async.set(AgsState.LoadingCandidates))
                                 candidates <- obsModeType
                                                 .map(ot =>
                                                   CatalogClient[IO]
                                                     .requestSingle:
                                                       CatalogMessage.GSRequest(
                                                         baseTracking,
                                                         siderealDiscretizedObsTime.obsTime,
                                                         ot
                                                       )
                                                 )
                                                 .getOrElse(none.pure[IO])
                               yield candidates)
                                 .guarantee:
                                   props.obsConf.flatMap(_.agsState).foldMap(_.async.set(AgsState.Idle))
                             else none.pure
      // Analysis results
      agsResults        <- useSerialState(List.empty[AgsAnalysis.Usable])
      // Reference to root
      root              <- useMemo(())(_ => domRoot)
      // target options, will be read from the user preferences
      options           <- useStateView(pending[AsterismVisualOptions])
      // Load target preferences
      targetPreferences <- useEffectWithDeps((props.uid, props.asterism.ids)): _ =>
                             import ctx.given

                             AsterismPreferences
                               .queryWithDefault[IO](props.uid,
                                                     props.asterism.ids,
                                                     Constants.InitialFov
                               )
                               .flatMap: tp =>
                                 (options.set(tp.ready) *>
                                   setVariable(root, "saturation", tp.saturation) *>
                                   setVariable(root, "brightness", tp.brightness)).toAsync
      // In case the selected name changes remotely
      _                 <- useEffectWithDeps(props.selectedGSName): n =>
                             // Go to the first analysis if present or pick the name from the selection
                             props.guideStarSelection.set:
                               n.fold(AgsSelection(agsResults.value.value.headOption.tupleLeft(0))):
                                 agsResults.value.value.pick
      // mouse coordinates, starts on the base
      mouseCoords       <- useState(props.asterism.baseTracking.baseCoordinates)
      // Reset offset and gs if asterism change
      _                 <- useEffectWithDeps(props.asterism): _ =>
                             val (_, offsetOnCenter) = offsetViews(props, options)(ctx)

                             // if the coordinates change, reset ags, offset and mouse coordinates
                             for
                               _ <- props.guideStarSelection.set(GuideStarSelection.Default)
                               _ <- agsResults.setState(List.empty)
                               _ <- offsetOnCenter.set(Offset.Zero)
                               _ <- mouseCoords.setState(props.asterism.baseTracking.baseCoordinates)
                             yield ()
      // Reset selection if pos angle changes except for manual selection changes
      _                 <- useEffectWithDeps(props.obsConf.flatMap(_.posAngleConstraint)): _ =>
                             (props.obsConf
                               .flatMap(_.agsState)
                               .foldMap(
                                 _.set(AgsState.Calculating)
                               ) *> props.guideStarSelection
                               .set(GuideStarSelection.Default))
                               .whenA(props.needsAGS && candidates.value.toOption.flatten.nonEmpty)
      // Request ags calculation
      _                 <- useEffectWithDeps(
                             (props.asterism.baseTracking,
                              props.asterism.focus.id,
                              props.positions,
                              props.obsConf.flatMap(_.posAngleConstraint),
                              props.obsConf.flatMap(_.constraints),
                              props.obsConf.flatMap(_.centralWavelength),
                              props.obsTime,
                              props.obsConf.flatMap(_.configuration),
                              candidates.value.toOption.flatten
                             )
                           ):
                             case (tracking,
                                   _,
                                   positions,
                                   _,
                                   Some(constraints),
                                   Some(centralWavelength),
                                   vizTime,
                                   observingMode,
                                   candidates
                                 ) if props.needsAGS && candidates.nonEmpty =>
                               import ctx.given

                               val runAgs: IO[Unit] =
                                 (positions,
                                  tracking.at(vizTime),
                                  props.obsConf.flatMap(_.obsModeType),
                                  props.obsConf.flatMap(_.agsState),
                                  candidates
                                 ).mapN { (positions, base, obsModeType, agsState, candidates) =>

                                   val params = obsModeType match {
                                     case ObservingModeType.Flamingos2LongSlit                                    =>
                                       val fpu = observingMode.collect {
                                         case m: BasicConfiguration.Flamingos2LongSlit =>
                                           m.fpu
                                       }
                                       fpu.map(f =>
                                         AgsParams.Flamingos2AgsParams(Flamingos2LyotWheel.F16,
                                                                       Flamingos2FpuMask.Builtin(f),
                                                                       PortDisposition.Side
                                         )
                                       )
                                     case ObservingModeType.GmosNorthLongSlit |
                                         ObservingModeType.GmosSouthLongSlit =>
                                       val fpu: Option[Either[GmosNorthFpu, GmosSouthFpu]] =
                                         observingMode.flatMap(_.gmosFpuAlternative)
                                       AgsParams.GmosAgsParams(fpu, PortDisposition.Side).some
                                     case ObservingModeType.GmosNorthImaging | ObservingModeType.GmosSouthImaging =>
                                       throw new NotImplementedError("Gmos Imaging not implemented")
                                   }

                                   val request: Option[AgsMessage.AgsRequest] =
                                     params.map(p =>
                                       AgsMessage.AgsRequest(
                                         props.asterism.focus.id,
                                         constraints,
                                         centralWavelength.value,
                                         base.value,
                                         props.sciencePositionsAt(vizTime),
                                         positions,
                                         p,
                                         candidates
                                       )
                                     )

                                   def processResults(r: Option[List[AgsAnalysis.Usable]]): IO[Unit] =
                                     (for
                                       // Store the analysis
                                       _ <- r.map(agsResults.setState).getOrEmpty
                                       // set the selected index to the first entry
                                       _ <-
                                         val index      = 0.some.filter(_ => r.exists(_.nonEmpty))
                                         val selectedGS = index.flatMap(i => r.flatMap(_.lift(i)))
                                         props.guideStarSelection
                                           .mod:
                                             case AgsSelection(_) =>
                                               AgsSelection(selectedGS.tupleLeft(0)) // replace automatic selection
                                             case rem @ RemoteGSSelection(name) =>
                                               // Recover the analysis for the remotely selected star
                                               // It is hydrating the name with the selection results
                                               r.map(_.pick(name)).getOrElse(rem)
                                             case a: AgsOverride                =>
                                               // If overriden ignore
                                               a
                                     yield ()).toAsync

                                   val process: IO[Unit] =
                                     for
                                       _ <- agsState.set(AgsState.Calculating).toAsync
                                       _ <-
                                         request
                                           .map(request =>
                                             AgsClient[IO]
                                               .requestSingle(request)
                                               .flatMap(processResults(_))
                                               .unlessA(candidates.isEmpty)
                                               .handleErrorWith(t => Logger[IO].error(t)("ERROR IN AGS REQUEST"))
                                           )
                                           .getOrElse(IO.unit)
                                     yield ()

                                   process.guarantee(agsState.async.set(AgsState.Idle))
                                 }.orEmpty

                               props.guideStarSelection
                                 .set(AgsSelection(none))
                                 .toAsync
                                 .whenA(positions.isEmpty) *>
                                 runAgs.unlessA(props.guideStarSelection.get.isOverride)
                             case _ => IO.unit
      menuRef           <- usePopupMenuRef
    } yield
      import ctx.given

      val fovView =
        options.zoom(Pot.readyPrism.andThen(fovLens))

      val globalPreferences = props.userPreferences.zoom(UserPreferences.globalPreferences)

      val fullScreenView =
        globalPreferences
          .zoom(GlobalPreferences.fullScreen)
          .withOnMod: v =>
            props.fullScreen.set(v) *> userPrefsSetter(props.uid, fullScreen = v.some)

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
        else
          fovView.set(newFov) *>
            AsterismPreferences
              .updateAladinPreferences[IO](
                options.get.toOption.flatMap(_.id),
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

      val (offsetChangeInAladin, offsetOnCenter) = offsetViews(props, options)(ctx)

      val guideStar = props.guideStarSelection.get.analysis

      val renderAladin: AsterismVisualOptions => VdomNode =
        (t: AsterismVisualOptions) =>
          AladinContainer(
            props.asterism,
            props.obsTime,
            props.obsConf.flatMap(ConfigurationForVisualization.fromObsConfiguration),
            globalPreferences.get,
            t,
            coordinatesSetter,
            fovSetter,
            offsetChangeInAladin.reuseAlways,
            guideStar,
            agsResults.value
          )

      val renderToolbar: (AsterismVisualOptions) => VdomNode =
        (t: AsterismVisualOptions) =>
          val agsState = props.obsConf
            .flatMap(_.agsState.map(_.get))
            .getOrElse(AgsState.Idle)
          AladinToolbar(
            Fov(t.fovRA, t.fovDec),
            mouseCoords.value,
            agsState,
            guideStar,
            globalPreferences.get.agsOverlay,
            offsetOnCenter
          )

      val renderAgsOverlay: AsterismVisualOptions => VdomNode =
        (_: AsterismVisualOptions) =>
          if (props.needsAGS && globalPreferences.get.agsOverlay)
            props.obsConf
              .flatMap(_.agsState)
              .map: agsState =>
                <.div(
                  ExploreStyles.AgsOverlay,
                  AgsOverlay(
                    props.guideStarSelection,
                    agsResults.value.filter(_.isUsable),
                    agsState.get,
                    props.modeSelected,
                    props.durationAvailable,
                    candidates.value.nonEmpty
                  )
                )
          else EmptyVdom

      <.div(ExploreStyles.TargetAladinCell)(
        <.div(
          ExploreStyles.AladinContainerColumn,
          AladinFullScreenControl(fullScreenView.zoom(fullScreenIso)),
          <.div(
            ExploreStyles.AladinToolbox,
            Button(onClickE = menuRef.toggle).withMods(
              ExploreStyles.ButtonOnAladin,
              Icons.ThinSliders
            )
          ),
          options.renderPotView(renderAladin),
          options.renderPotView(renderToolbar),
          options.renderPotView(renderAgsOverlay)
        ),
        options
          .zoom(Pot.readyPrism[AsterismVisualOptions])
          .mapValue: options =>
            AladinPreferencesMenu(
              props.uid,
              props.asterism.ids,
              globalPreferences,
              options,
              menuRef
            )
      )
