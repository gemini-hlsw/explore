// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all._
import crystal.react.View
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import explore.Icons
import explore.common.UserPreferencesQueries._
import explore.common.UserPreferencesQueriesGQL._
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.ModelOptics
import explore.model.TargetVisualOptions
import explore.model.reusability._
import japgolly.scalajs.react.ReactMonocle._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.ui.reusability._
import monocle.Focus
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
  target:           View[Coordinates],
  options:          View[TargetVisualOptions]
)(implicit val ctx: AppContextIO)
    extends ReactProps[AladinCell](AladinCell.component) {
  val aladinCoords: Coordinates = target.get
}

object AladinCell extends ModelOptics {
  type Props = AladinCell
  val AladinRef = AladinContainer.component

  final case class State(fov: Fov, current: Coordinates)

  object State {
    val fov     = Focus[State](_.fov)
    val current = Focus[State](_.current)
    val Zero    = State(Fov(Angle.Angle0, Angle.Angle0), Coordinates.Zero)
  }
  implicit val propsReuse = Reusability.derive[Props]
  implicit val stateReuse = Reusability.never[State]

  class Backend($ : BackendScope[Props, State]) {
    // Create a mutable reference
    private val aladinRef = Ref.toScalaComponent(AladinRef)

    val centerOnTarget =
      aladinRef.get.asCBO
        .flatMapCB(_.backend.centerOnTarget)
        .toCallback

    val gotoRaDec = (coords: Coordinates) =>
      $.setStateL(State.current)(coords) *>
        aladinRef.get.asCBO
          .flatMapCB(_.backend.gotoRaDec(coords))
          .toCallback

    val coordinatesSetter =
      ((coords: Coordinates) => $.setStateL(State.current)(coords)).reuseAlways

    def fovSetter(props: Props, fov: Fov): Callback =
      if (fov.x.toMicroarcseconds === 0L) Callback.empty
      else {
        implicit val ctx = props.ctx
        $.setStateL(State.fov)(fov) >>
          UserTargetPreferencesUpsert
            .updateFov[IO](props.uid, props.tid, fov.x)
            .runAsyncAndForget
            .debounce(1.seconds)
      }

    def render(props: Props, state: State) =
      React.Fragment(
        <.div(
          ExploreStyles.TargetAladinCell,
          <.div(
            ExploreStyles.AladinContainerColumn,
            AladinRef
              .withRef(aladinRef) {
                AladinContainer(
                  props.target,
                  props.options.get,
                  coordinatesSetter,
                  Reuse.currying(props).in(fovSetter _)
                )
              },
            AladinToolbar(state.fov, state.current),
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

    def newProps(currentProps: Props, nextProps: Props): Callback =
      gotoRaDec(nextProps.aladinCoords)
        .when(nextProps.aladinCoords =!= currentProps.aladinCoords)
        .void
  }

  val component =
    ScalaComponent
      .builder[Props]
      .initialStateFromProps(p => State.Zero.copy(current = p.aladinCoords))
      .renderBackend[Backend]
      .componentDidUpdate($ => $.backend.newProps($.prevProps, $.currentProps))
      .configure(Reusability.shouldComponentUpdate)
      .build

}
