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
import react.aladin.reusability._
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
  configuration:    Option[ScienceConfiguration],
  target:           ReuseView[Coordinates],
  options:          ReuseView[TargetVisualOptions]
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
      // field of view
      .useStateViewWithReuseBy((p, _) => Fov(p.options.get.fovAngle, p.options.get.fovAngle))
      // flag to trigger centering. This is a bit brute force but
      // avoids us needing a ref to a Fn component
      .useStateViewWithReuse(false)
      .renderWithReuse { (props, mouseCoords, fov, center) =>
        val coordinatesSetter =
          ((c: Coordinates) => mouseCoords.setState(c)).reuseAlways

        def fovSetter(props: Props, newFov: Fov): Callback =
          if (newFov.x.toMicroarcseconds === 0L) Callback.empty
          else {
            implicit val ctx = props.ctx
            UserTargetPreferencesUpsert
              .updateFov[IO](props.uid, props.tid, newFov.x)
              .runAsyncAndForget
              .debounce(1.seconds)
          }

        <.div(
          ExploreStyles.TargetAladinCell,
          <.div(
            ExploreStyles.AladinContainerColumn,
            AladinContainer(
              props.target,
              props.options.get,
              props.configuration,
              fov,
              coordinatesSetter,
              Reuse.currying(props).in(fovSetter _),
              center
            ).withKey(props.aladinCoords.toString),
            AladinToolbar(fov.get, mouseCoords.value),
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
