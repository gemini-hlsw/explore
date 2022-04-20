// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all._
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
import explore.model.ScienceConfiguration
import explore.model.TargetVisualOptions
import explore.model.reusability._
import explore.optics.ModelOptics
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.Coordinates
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
import crystal.Pot
import crystal.implicits._

final case class AladinCell(
  uid:              User.Id,
  tid:              Target.Id,
  configuration:    Option[ScienceConfiguration],
  target:           ReuseView[Coordinates]
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[AladinCell](AladinCell.component) {
  val aladinCoords: Coordinates = target.get
}

object AladinCell extends ModelOptics {
  type Props = AladinCell

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  val component =
    ScalaFnComponent
      .withHooks[Props]
      // mouse coordinates, starts on the base
      .useStateBy(_.aladinCoords)
      // target options, will be read from the user preferences
      .useState(Pot.pending[TargetVisualOptions])
      // flag to trigger centering. This is a bit brute force but
      // avoids us needing a ref to a Fn component
      .useStateViewWithReuse(false)
      .useEffectWithDepsBy((p, _, _, _) => (p.uid, p.tid)) { (props, _, options, _) => _ =>
        implicit val ctx = props.ctx
        UserTargetPreferencesQuery
          .queryWithDefault[IO](props.uid, props.tid, Constants.InitialFov)
          .flatMap(fov =>
            options.setState(TargetVisualOptions.Default.copy(fov = fov).ready).to[IO]
          )
          .runAsyncAndForget
      }
      .renderWithReuse { (props, mouseCoords, options, center) =>
        val coordinatesSetter =
          ((c: Coordinates) => mouseCoords.setState(c)).reuseAlways

        val fov: Option[Fov] =
          options.value.fold(_ => none, _ => none, tv => Fov(tv.fov, tv.fov).some)

        def fovSetter(props: Props, newFov: Fov): Callback =
          if (newFov.x.toMicroarcseconds === 0L) Callback.empty
          else {
            implicit val ctx = props.ctx
            options.modState(_.map(_.copy(fov = newFov.x))) *>
              UserTargetPreferencesUpsert
                .updateFov[IO](props.uid, props.tid, newFov.x)
                .whenA(fov.forall(_.x != newFov.x))
                .runAsync
                .rateLimit(1.seconds, 1)
                .void
          }

        <.div(
          ExploreStyles.TargetAladinCell,
          <.div(
            ExploreStyles.AladinContainerColumn,
            fov.map(fov =>
              AladinContainer(
                props.target,
                props.configuration,
                fov,
                coordinatesSetter,
                Reuse.currying(props).in(fovSetter _),
                center
              ).withKey(props.aladinCoords.toString)
            ),
            fov.map(AladinToolbar(_, mouseCoords.value)),
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
            )
          )
        )
      }

}
