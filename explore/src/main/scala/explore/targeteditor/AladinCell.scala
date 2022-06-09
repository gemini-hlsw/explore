// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all._
import crystal.Pot
import crystal.Ready
import crystal.implicits._
import crystal.react.View
import crystal.react.hooks._
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import explore.Icons
import explore.common.UserPreferencesQueries._
import explore.components.ui.ExploreStyles
import explore.events._
import explore.events.picklers._
import explore.implicits._
import explore.model.CatalogQueryError
import explore.model.CatalogResults
import explore.model.Constants
import explore.model.ObsConfiguration
import explore.model.ScienceMode
import explore.model.TargetVisualOptions
import explore.model.boopickle._
import explore.model.enum.Visible
import explore.model.reusability._
import explore.optics.ModelOptics
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ags._
import lucuma.core.enum.CloudExtinction
import lucuma.core.enum.ImageQuality
import lucuma.core.enum.PortDisposition
import lucuma.core.enum.SkyBackground
import lucuma.core.enum.WaterVapor
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.PosAngle
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.ui.reusability._
import queries.common.UserPreferencesQueriesGQL._
import react.aladin.Fov
import react.common._
import react.semanticui.collections.menu._
import react.semanticui.elements.button.Button
import react.semanticui.modules.checkbox.Checkbox
import react.semanticui.sizes._

import scala.concurrent.duration._

final case class AladinCell(
  uid:              User.Id,
  tid:              Target.Id,
  obsConf:          Option[ObsConfiguration],
  scienceMode:      Option[ScienceMode],
  target:           View[SiderealTracking]
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[AladinCell](AladinCell.component)

final case class AladinSettings(showMenu: Boolean, showCatalog: Boolean)

object AladinSettings {
  val Default = AladinSettings(false, false)
}

object AladinCell extends ModelOptics {
  type Props = AladinCell

  val bestConstraintSet = ConstraintSet(ImageQuality.PointTwo,
                                        CloudExtinction.PointOne,
                                        SkyBackground.Darkest,
                                        WaterVapor.VeryDry,
                                        ElevationRange.AirMass.Default
  )
  val wavelength        = Wavelength.fromNanometers(500).get

  val params  = AgsParams.GmosAgsParams(none, PortDisposition.Side)
  val basePos = AgsPosition(Angle.Angle0, Offset.Zero)

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      // mouse coordinates, starts on the base
      .useStateBy(_.target.get.baseCoordinates)
      // target options, will be read from the user preferences
      .useStateView(Pot.pending[TargetVisualOptions])
      // flag to trigger centering. This is a bit brute force but
      // avoids us needing a ref to a Fn component
      .useStateView(false)
      // Listen on web worker for messages with catalog candidates
      .useStreamWithSyncBy((props, _, _, _) => props.tid)((props, _, _, _) =>
        _ =>
          props.ctx.worker.stream
            .flatMap { r =>
              val resultsOrError = decodeFromTransferable[CatalogResults](r)
                .map(_.asRight)
                .orElse(
                  decodeFromTransferable[CatalogQueryError](r).map(_.asLeft)
                )
              resultsOrError match {
                case Some(Right(r)) => fs2.Stream.emit[IO, CatalogResults](r)
                case Some(Left(m))  => fs2.Stream.raiseError[IO](new RuntimeException(m.errorMsg))
                case _              => fs2.Stream.raiseError[IO](new RuntimeException("Unknown worker message"))
              }
            }
            .map(_.candidates)
      )
      .useEffectWithDepsBy((_, _, _, _, candidates) => candidates.awaitOpt)((props, _, _, _, _) =>
        _.value
          .map(candidatesAwait =>
            candidatesAwait >>
              ((props.target.get, props.obsConf) match {
                case (tracking, Some(obsConf)) =>
                  props.ctx.worker.postTransferrable(CatalogRequest(tracking, obsConf.obsInstant))
                case _                         => IO.unit
              })
          )
          .orEmpty
      )
      .useEffectWithDepsBy((p, _, _, _, _) => (p.uid, p.tid)) { (props, _, options, _, _) => _ =>
        implicit val ctx = props.ctx
        UserTargetPreferencesQuery
          .queryWithDefault[IO](props.uid, props.tid, Constants.InitialFov)
          .flatMap { case (fov, viewOffset, agsCandidates, agsOverlay) =>
            options
              .set(
                TargetVisualOptions.Default
                  .copy(fovAngle = fov,
                        viewOffset = viewOffset,
                        agsCandidates = agsCandidates,
                        agsOverlay = agsOverlay
                  )
                  .ready
              )
              .to[IO]
          }
          .runAsyncAndForget
      }
      // analyzed targets
      .useMemoBy((p, _, _, _, candidates) => (p.target.get, p.obsConf, candidates.value)) {
        (_, _, _, _, _) =>
          {
            case (tracking, Some(obsConf), Ready(candidates)) =>
              val pa = obsConf.posAngle match {
                case PosAngle.Fixed(a)               => a.some
                case PosAngle.AllowFlip(a)           => a.some
                case PosAngle.ParallacticOverride(a) => a.some
                case _                               => none
              }

              pa.map { pa =>
                val basePos = AgsPosition(pa, Offset.Zero)
                Ags
                  .agsAnalysis[IO](bestConstraintSet,
                                   wavelength,
                                   tracking.baseCoordinates,
                                   basePos,
                                   params,
                                   candidates
                  )
                  .sortBy(_._2)

              }.getOrElse(Nil)
            case _                                            => Nil
          }
      }
      // open settings menu
      .useState(false)
      // Selected GS index. Should be stored in the db
      .useStateView(0)
      .render {
        (props, mouseCoords, options, center, gsc, agsResults, openSettings, selectedIndex) =>
          implicit val ctx = props.ctx

          val agsCandidatesView =
            options.zoom(Pot.readyPrism.andThen(TargetVisualOptions.agsCandidates))

          val agsOverlayView =
            options.zoom(Pot.readyPrism.andThen(TargetVisualOptions.agsOverlay))

          val fovView =
            options.zoom(Pot.readyPrism.andThen(TargetVisualOptions.fovAngle))

          val offsetView =
            options.zoom(Pot.readyPrism.andThen(TargetVisualOptions.viewOffset))

          val agsCandidatesShown: Boolean = agsCandidatesView.get.map(_.visible).getOrElse(false)

          val agsOverlayShown: Boolean = agsOverlayView.get.map(_.visible).getOrElse(false)

          val coordinatesSetter =
            ((c: Coordinates) => mouseCoords.setState(c)).reuseAlways

          val fovSetter = (newFov: Fov) => {
            val ignore = options.get.fold(
              _ => true,
              _ => true,
              o =>
                // Don't save if the change is less than 1 arcse
                (o.fovAngle.toMicroarcseconds - newFov.x.toMicroarcseconds).abs < 1e6
            )
            if (newFov.x.toMicroarcseconds === 0L) Callback.empty
            else {
              fovView.set(newFov.x) *>
                (fovView.get, agsCandidatesView.get, agsOverlayView.get).mapN { (_, a, o) =>
                  UserTargetPreferencesUpsert
                    .updateAladinPreferences[IO](props.uid, props.tid, newFov.x, a, o)
                    .unlessA(ignore)
                    .runAsync
                    .rateLimit(1.seconds, 1)
                    .void
                }.orEmpty
            }
          }

          val offsetSetter = (newOffset: Offset) => {
            val ignore = options.get.fold(
              _ => true,
              _ => true,
              o => {
                val diffP = newOffset.p.toAngle.difference(o.viewOffset.p.toAngle)
                val diffQ = newOffset.q.toAngle.difference(o.viewOffset.q.toAngle)
                // Don't save if the change is less than 1 arcse
                diffP.toMicroarcseconds < 1e6 && diffQ.toMicroarcseconds < 1e6
              }
            )

            offsetView.set(newOffset) *>
              UserTargetPreferencesFovUpdate
                .updateViewOffset[IO](props.uid, props.tid, newOffset)
                .unlessA(ignore)
                .runAsync
                .rateLimit(1.seconds, 1)
                .void
          }

          def prefsSetter(candidates: Visible => Visible, overlay: Visible => Visible): Callback =
            (fovView.get, agsCandidatesView.get, agsOverlayView.get).mapN { (f, a, o) =>
              UserTargetPreferencesUpsert
                .updateAladinPreferences[IO](props.uid, props.tid, f, candidates(a), overlay(o))
                .runAsync
                .void
            }.orEmpty

          def agsOverlaySetter: Callback =
            agsOverlayView.mod(_.flip) *>
              prefsSetter(identity, _.flip)

          def candidatesSetter: Callback =
            agsCandidatesView.mod(_.flip) *>
              prefsSetter(_.flip, identity)

          val aladinKey = s"key-${props.target.get}"

          val renderCell: TargetVisualOptions => VdomNode = (t: TargetVisualOptions) =>
            AladinContainer(
              props.target,
              props.obsConf,
              props.scienceMode,
              t,
              coordinatesSetter,
              fovSetter.reuseAlways,
              offsetSetter.reuseAlways,
              center,
              selectedIndex.get,
              agsResults
            ).withKey(aladinKey)

          // Check whether we are waiting for catalog
          val catalogLoading = props.obsConf match {
            case Some(_) =>
              gsc.value.fold(_ => true.some, _ => none, _ => false.some)
            case _       => false.some
          }

          val usableGuideStar = agsResults.lift(selectedIndex.get).exists(_._2.isUsable)

          val renderToolbar: TargetVisualOptions => VdomNode =
            (t: TargetVisualOptions) =>
              AladinToolbar(Fov.square(t.fovAngle),
                            mouseCoords.value,
                            catalogLoading,
                            selectedIndex,
                            agsResults,
                            center,
                            t.agsOverlay
              ): VdomNode

          val renderAgsOverlay: TargetVisualOptions => VdomNode =
            (t: TargetVisualOptions) =>
              if (t.agsOverlay.visible && usableGuideStar) {
                <.div(
                  ExploreStyles.AgsOverlay,
                  AgsOverlay(
                    selectedIndex,
                    agsResults
                  )
                )
              } else EmptyVdom

          <.div(
            ExploreStyles.TargetAladinCell,
            <.div(
              ExploreStyles.AladinContainerColumn,
              <.div(
                ExploreStyles.AladinToolbox,
                Button(size = Small, icon = true, onClick = openSettings.modState(s => !s))(
                  ExploreStyles.ButtonOnAladin,
                  ^.onMouseEnter --> openSettings.setState(true),
                  Icons.ThinSliders
                ),
                Menu(vertical = true,
                     compact = true,
                     size = Mini,
                     clazz = ExploreStyles.AladinSettingsMenu
                )(
                  ^.onMouseLeave --> openSettings.setState(false),
                  MenuItem(
                    Checkbox(
                      label = "Show Catalog",
                      checked = agsCandidatesShown,
                      onChange = (_: Boolean) => openSettings.setState(false) *> candidatesSetter
                    )
                  ),
                  MenuItem(
                    Checkbox(
                      label = "AGS",
                      checked = agsOverlayShown,
                      onChange = (_: Boolean) => openSettings.setState(false) *> agsOverlaySetter
                    )
                  )
                ).when(openSettings.value)
              ),
              potRenderView[TargetVisualOptions](renderCell)(options),
              potRenderView[TargetVisualOptions](renderToolbar)(options),
              potRenderView[TargetVisualOptions](renderAgsOverlay)(options)
            )
          )
      }

}
