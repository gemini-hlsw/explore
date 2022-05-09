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
import explore.implicits._
import explore.model.Constants
import explore.model.ObsConfiguration
import explore.model.ScienceMode
import explore.model.TargetVisualOptions
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
import queries.common.UserPreferencesQueriesGQL._
import react.aladin.Fov
import react.common._
import react.fa.Transform
import react.semanticui.elements.button.Button
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
      .useEffectWithDepsBy((p, _, _, _) => (p.uid, p.tid)) { (props, _, options, _) => _ =>
        implicit val ctx = props.ctx
        UserTargetPreferencesQuery
          .queryWithDefault[IO](props.uid, props.tid, Constants.InitialFov)
          .flatMap { case (fov, viewOffset) =>
            options
              .set(
                TargetVisualOptions.Default.copy(fovAngle = fov, viewOffset = viewOffset).ready
              )
              .to[IO]
          }
          .runAsyncAndForget
      }
      .renderWithReuse { (props, mouseCoords, options, center) =>
        val coordinatesSetter =
          ((c: Coordinates) => mouseCoords.setState(c)).reuseAlways

        def fovSetter(props: Props, newFov: Fov): Callback = {
          val ignore = options.get.fold(
            _ => true,
            _ => true,
            o =>
              // Don't save if the change is less than 1 arcse
              (o.fovAngle.toMicroarcseconds - newFov.x.toMicroarcseconds).abs < 1e6
          )
          if (ignore || newFov.x.toMicroarcseconds === 0L) Callback.empty
          else {
            implicit val ctx = props.ctx
            options.mod(_.map(_.copy(fovAngle = newFov.x))) *>
              UserTargetPreferencesUpsert
                .updateFov[IO](props.uid, props.tid, newFov.x)
                .runAsync
                .rateLimit(1.seconds, 1)
                .void
          }
        }

        def offsetSetter(props: Props, newOffset: Offset): Callback = {
          implicit val ctx = props.ctx
          options.mod(_.map(_.copy(viewOffset = newOffset))) *>
            UserTargetPreferencesFovUpdate
              .updateViewOffset[IO](props.uid, props.tid, newOffset)
              .runAsync
              .rateLimit(1.seconds, 1)
              .void
        }

        val aladinKey = s"${props.target.get}"

        val renderCell: TargetVisualOptions => VdomNode = (t: TargetVisualOptions) =>
          AladinContainer(
            props.target,
            props.obsConf,
            props.scienceMode,
            t,
            coordinatesSetter,
            Reuse.currying(props).in(fovSetter _),
            Reuse.currying(props).in(offsetSetter _),
            center
          ).withKey(aladinKey)

        val renderToolbar: TargetVisualOptions => VdomNode =
          (t: TargetVisualOptions) =>
            AladinToolbar(Fov.square(t.fovAngle), mouseCoords.value): VdomNode

        <.div(
          ExploreStyles.TargetAladinCell,
          <.div(
            ExploreStyles.AladinContainerColumn,
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
