// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all._
import crystal.ViewF
import crystal.react.implicits._
import eu.timepit.refined._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.collection._
import eu.timepit.refined.types.string.NonEmptyString
import explore.AppCtx
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.ModelOptics._
import explore.model.SiderealTarget
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.ui.forms._
import monocle.macros.Lenses
import react.common._
import react.common.implicits._
import react.semanticui.collections.form._
import react.semanticui.elements.icon.Icon
import react.semanticui.modules.dropdown.DropdownItem
import react.semanticui.sizes._

final case class CoordinatesForm(
  target:           SiderealTarget,
  searchAndGo:      NonEmptyString => Callback,
  goToRaDec:        Coordinates => Callback
)(implicit val ctx: AppContextIO)
    extends ReactProps[CoordinatesForm](CoordinatesForm.component) {
  def submit(searchTerm: String): Callback =
    refineV[NonEmpty](searchTerm)
      .fold(_ => Callback.empty, s => searchAndGo(s).when_(s =!= target.name))
}

object CoordinatesForm {
  type Props = CoordinatesForm

  @Lenses
  final case class State(
    searchTerm: String,
    raValue:    RightAscension,
    decValue:   Declination
  )

  def initialState(p: Props): State = {
    val r = targetPropsL.get(p.target)
    Function.tupled(State.apply _)((r._1.value, r._2, r._3))
  }

  class Backend($ : BackendScope[Props, State]) {

    def render(props: Props, state: State) =
      AppCtx.withCtx { implicit appCtx =>
        val stateView = ViewF.fromState[IO]($)

        Form(
          size = Small,
          onSubmit = props.submit(state.searchTerm)
        )(
          ExploreStyles.Grid,
          ExploreStyles.Compact,
          ExploreStyles.CoordinatesForm,
          FormDropdown(
            label = "Type",
            value = 0,
            selection = true,
            options = List(DropdownItem(value = 0, text = "Sidereal"),
                           DropdownItem(value = 1, text = "Non-sidereal")
            )
          ),
          FormInputEV(id = "search",
                      value = stateView.zoom(State.searchTerm),
                      label = "Target",
                      focus = true,
                      icon = Icon("search")
          ),
          <.div(
            ExploreStyles.FlexContainer,
            ExploreStyles.TargetRaDecMinWidth,
            FormInputEV(
              id = "ra",
              value = stateView.zoom(State.raValue),
              format = RightAscension.fromStringHMS,
              label = "RA",
              clazz = ExploreStyles.Grow(1) |+| ExploreStyles.TargetRaDecMinWidth
            ),
            FormInputEV(
              id = "dec",
              value = stateView.zoom(State.decValue),
              format = Declination.fromStringSignedDMS,
              label = "Dec",
              clazz = ExploreStyles.Grow(1) |+| ExploreStyles.TargetRaDecMinWidth
            ),
            FormButton(
              size = Small,
              icon = true,
              label = "Go To",
              onClick = props.goToRaDec(Coordinates(state.raValue, state.decValue))
            )(
              Icon("angle right"),
              ExploreStyles.HideLabel
            )
          )
        )
      }
  }

  val component =
    ScalaComponent
      .builder[Props]
      .initialStateFromProps(initialState(_))
      .renderBackend[Backend]
      .build

}
