// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all._
import crystal.react.ReuseView
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import explore.Icons
import explore.common.UserPreferencesQueries._
import explore.common.UserPreferencesQueriesGQL._
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.TargetVisualOptions
import explore.model.reusability._
import explore.optics.ModelOptics
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.ui.reusability._
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
  target:           ReuseView[Coordinates],
  options:          ReuseView[TargetVisualOptions]
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[AladinCell](AladinCell.component) {
  val aladinCoords: Coordinates = target.get
}

object AladinCell extends ModelOptics {
  type Props = AladinCell
  val AladinRef = AladinContainer.component

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateBy(_.aladinCoords)
      .useState(Fov(Angle.Angle0, Angle.Angle0))
      .useRefToScalaComponent(AladinContainer.component)
      // Waiting on how to make a Reusability to the ref
      .render { (props, coords, fov, aladinRef) =>
        val coordinatesSetter =
          ((c: Coordinates) => coords.setState(c)).reuseAlways

        def fovSetter(props: Props, newFov: Fov): Callback =
          if (newFov.x.toMicroarcseconds === 0L) Callback.empty
          else {
            implicit val ctx = props.ctx
            fov.setState(newFov) >>
              UserTargetPreferencesUpsert
                .updateFov[IO](props.uid, props.tid, newFov.x)
                .runAsyncAndForget
                .debounce(1.seconds)
          }

        val centerOnTarget =
          aladinRef.get.asCBO
            .flatMapCB(_.backend.centerOnTarget)
            .toCallback

        React.Fragment(
          <.div(
            ExploreStyles.TargetAladinCell,
            <.div(
              ExploreStyles.AladinContainerColumn,
              // AladinRef
              //   .withRef(aladinRef)
              //   .withKey(
              //     props.target.get.toString
              // ) {
              // AladinContainer(
              //   props.target,
              //   props.options.get,
              //   coordinatesSetter,
              //   Reuse.currying(props).in(fovSetter _)
              // )
              // },
              AladinToolbar(fov.value, coords.value),
              <.div(
                ExploreStyles.AladinCenterButton,
                Popup(
                  content = "Center on target",
                  position = PopupPosition.BottomLeft,
                  trigger = Button(size = Mini, icon = true, onClick = centerOnTarget)(
                    Icons.Bullseye
                      .transform(Transform(size = 24))
                      .clazz(ExploreStyles.Accented)
                  )
                )
              )
            )
          )
        )
      }

}
