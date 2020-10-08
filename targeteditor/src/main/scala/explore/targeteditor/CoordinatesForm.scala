// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all._
import crystal.ViewF
import crystal.react.implicits._
import eu.timepit.refined._
import eu.timepit.refined.auto._
import eu.timepit.refined.collection._
import explore.AppCtx
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.Icons
import explore.model.ModelOptics._
import explore.model.SiderealTarget
import explore.model.reusability._
import explore.utils.abbreviate
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.Target
import lucuma.ui.forms._
import lucuma.ui.reusability._
import monocle.macros.Lenses
import react.common._
import react.common.implicits._
import react.semanticui.collections.form._
import react.semanticui.elements.icon.Icon
import react.semanticui.elements.label._
import react.semanticui.modules.dropdown.DropdownItem
import react.semanticui.sizes._

final case class CoordinatesForm(
  target:           SiderealTarget,
  searchAndGo:      SearchCallback => Callback,
  goToRaDec:        Coordinates => Callback
)(implicit val ctx: AppContextIO)
    extends ReactProps[CoordinatesForm](CoordinatesForm.component) {
  def submit(
    searchTerm: String,
    before:     Callback,
    onComplete: Option[Target] => Callback,
    onError:    Throwable => Callback
  ): Callback =
    refineV[NonEmpty](searchTerm)
      .fold(_ => Callback.empty, s => before *> searchAndGo(SearchCallback(s, onComplete, onError)))
}

object CoordinatesForm {
  type Props = CoordinatesForm

  @Lenses
  final case class State(
    searchTerm:  String,
    raValue:     RightAscension,
    decValue:    Declination,
    searching:   Boolean,
    searchError: Option[String]
  )

  def initialState(p: Props): State = {
    val r = targetPropsL.get(p.target)
    Function.tupled(State.apply _)((r._1.value, r._2, r._3, false, none))
  }

  implicit val stateReuse                     = Reusability.derive[State]
  implicit val propsReuse: Reusability[Props] = Reusability.by(_.target)

  class Backend($ : BackendScope[Props, State]) {

    def render(props: Props, state: State) =
      AppCtx.withCtx { implicit appCtx =>
        val stateView = ViewF.fromState[IO]($)

        val searchComplete: Callback = $.setStateL(State.searching)(false)

        val search: Callback =
          props
            .submit(
              state.searchTerm,
              $.setStateL(State.searching)(true),
              t =>
                searchComplete *> ($.setStateL(State.searchError)(
                  s"'${abbreviate(state.searchTerm, 10)}' not found".some
                )).when_(t.isEmpty),
              t =>
                searchComplete *> $.setStateL(State.searchError)(
                  s"Search error ${abbreviate(t.getMessage, 10)}".some
                )
            )

        def iconKeyPress(e: ReactKeyboardEvent): Callback =
          search *> e.stopPropagationCB *> e.preventDefaultCB

        val nameLabel = state.searchError match {
          case Some(m) => Label(clazz = ExploreStyles.ErrorLabel)(m)
          case _       => Label("Name")
        }

        Form(size = Small)(
          ExploreStyles.Grid,
          ExploreStyles.Compact,
          ExploreStyles.CoordinatesForm,
          FormSelect(
            label = "Type",
            value = 0,
            options = List(DropdownItem(value = 0, text = "Sidereal"),
                           DropdownItem(value = 1, text = "Non-sidereal")
            )
          ),
          FormInputEV(
            id = "search",
            value = stateView.zoom(State.searchTerm),
            label = nameLabel,
            focus = true,
            loading = state.searching,
            onChange = (_: String) => $.setStateL(State.searchError)(none),
            icon = Icons.Search
              .link(true)
              .clazz(ExploreStyles.ButtonIcon)(
                ^.tabIndex := 0,
                ^.onKeyPress ==> iconKeyPress,
                ^.onClick --> search
              )
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
      .initialStateFromProps(initialState)
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .build

}
