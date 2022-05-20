// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all._
import crystal.Pot
import crystal.implicits._
import crystal.react.ReuseView
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
import explore.model.CatalogResults
import explore.model.Constants
import explore.model.GuideStarCandidate
import explore.model.ObsConfiguration
import explore.model.ScienceMode
import explore.model.TargetVisualOptions
import explore.model.boopickle._
import explore.model.reusability._
import explore.optics.ModelOptics
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.ui.reusability._
import monocle.Prism
import queries.common.UserPreferencesQueriesGQL._
import react.aladin.Fov
import react.common._
import react.fa.Transform
import react.semanticui.collections.menu._
import react.semanticui.elements.button.Button
import react.semanticui.modules.checkbox.Checkbox
import react.semanticui.modules.popup.Popup
import react.semanticui.modules.popup.PopupPosition
import react.semanticui.sizes._

import scala.concurrent.duration._

final case class AladinCell(
  uid:              User.Id,
  tid:              Target.Id,
  obsConf:          Option[ObsConfiguration],
  scienceMode:      Option[ScienceMode],
  target:           ReuseView[SiderealTracking]
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[AladinCell](AladinCell.component)

final case class AladinSettings(showMenu: Boolean, showCatalog: Boolean)

object AladinSettings {
  val Default = AladinSettings(false, false)
}

object AladinCell extends ModelOptics {
  type Props = AladinCell

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  val component =
    ScalaFnComponent
      .withHooks[Props]
      // mouse coordinates, starts on the base
      .useStateBy(_.target.get.baseCoordinates)
      // target options, will be read from the user preferences
      .useStateViewWithReuse(Pot.pending[TargetVisualOptions])
      // flag to trigger centering. This is a bit brute force but
      // avoids us needing a ref to a Fn component
      .useStateViewWithReuse(false)
      // GuideStar candidates
      .useState(List.empty[GuideStarCandidate])
      // Lisen on web worker for messages with catalog candidates
      .useEffectOnMountBy((props, _, _, _, gs) =>
        props.ctx.worker.stream
          .evalMap(m =>
            decodeFromTransferable[IO, CatalogResults](m)(gsl => gs.setState(gsl.candidates).to[IO])
          )
          .compile
          .drain
          .whenA(props.obsConf.isDefined)
      )
      // Request catalog info for the target
      .useEffectWithDepsBy((props, _, _, _, _) => (props.target.get, props.obsConf))(
        (props, _, _, _, _) => {
          case (tracking, Some(obsConf)) =>
            props.ctx.worker
              .postTransferrable(CatalogRequest(tracking, obsConf.obsInstant))
          case _                         => IO.unit
        }
      )
      .useEffectWithDepsBy((p, _, _, _, _) => (p.uid, p.tid)) { (props, _, options, _, _) => _ =>
        implicit val ctx = props.ctx
        UserTargetPreferencesQuery
          .queryWithDefault[IO](props.uid, props.tid, Constants.InitialFov)
          .flatMap { case (fov, viewOffset, agsCandidates) =>
            options
              .set(
                TargetVisualOptions.Default
                  .copy(fovAngle = fov, viewOffset = viewOffset, agsCandidates = agsCandidates)
                  .ready
              )
              .to[IO]
          }
          .runAsyncAndForget
      }
      // open settings menu
      .useState(false)
      .renderWithReuse { (props, mouseCoords, options, center, gsc, openSettings) =>
        implicit val ctx = props.ctx

        def potPrism[A]: Prism[Pot[A], A] =
          Prism.apply[Pot[A], A](_.toOption)(_.ready)

        val agsCandidatesView =
          options.zoom(potPrism.andThen(TargetVisualOptions.agsCandidates))

        val fovView =
          options.zoom(potPrism.andThen(TargetVisualOptions.fovAngle))

        val offsetView =
          options.zoom(potPrism.andThen(TargetVisualOptions.viewOffset))

        val agsCandidatesShown: Boolean = agsCandidatesView.get.map(_.visible).getOrElse(false)

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
              (fovView.get, agsCandidatesView.get).mapN { (_, a) =>
                UserTargetPreferencesUpsert
                  .updatePreferences[IO](props.uid, props.tid, newFov.x, a)
                  .unlessA(ignore)
                  .runAsync
                  .rateLimit(1.seconds, 1)
                  .void
              }.orEmpty
          }
        }

        val offsetSetter = (newOffset: Offset) =>
          offsetView.set(newOffset) *>
            UserTargetPreferencesFovUpdate
              .updateViewOffset[IO](props.uid, props.tid, newOffset)
              .runAsync
              .rateLimit(1.seconds, 1)
              .void

        def candidatesSetter: Callback =
          agsCandidatesView.mod(_.flip) *>
            (fovView.get, agsCandidatesView.get).mapN { (f, a) =>
              UserTargetPreferencesUpsert
                .updatePreferences[IO](props.uid, props.tid, f, a.flip)
                .runAsync
                .void
            }.orEmpty

        val aladinKey = s"${props.target.get}"

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
            gsc.value
          ).withKey(aladinKey)

        val renderToolbar: TargetVisualOptions => VdomNode =
          (t: TargetVisualOptions) =>
            AladinToolbar(Fov.square(t.fovAngle), mouseCoords.value): VdomNode

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
                )
              ).when(openSettings.value)
            ),
            potRender[TargetVisualOptions](renderCell.reuseAlways)(options.get),
            potRender[TargetVisualOptions](renderToolbar.reuseAlways)(options.get),
            <.div(
              ExploreStyles.AladinCenterButton,
              Popup(
                content = "Center on target",
                position = PopupPosition.BottomLeft,
                trigger = Button(size = Mini, icon = true, onClick = center.set(true))(
                  Icons.Bullseye
                    .transform(Transform(size = 24))
                    .clazz(ExploreStyles.Accented)
                )
              )
            ).when(options.get.toOption.isDefined)
          )
        )
      }

}
