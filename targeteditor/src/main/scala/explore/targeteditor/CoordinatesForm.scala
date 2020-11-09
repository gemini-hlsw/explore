// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.data._
import cats.effect.IO
import cats.syntax.all._
import crystal.ViewF
import crystal.react.implicits._
import eu.timepit.refined._
import eu.timepit.refined.auto._
import eu.timepit.refined.collection._
import eu.timepit.refined.types.string.NonEmptyString
import explore.AppCtx
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.target.TargetQueries._
import explore.utils.abbreviate
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.ui.forms._
import lucuma.ui.optics.ValidFormatInput
import lucuma.ui.reusability._
import monocle.macros.Lenses
import react.common._
import react.common.implicits._
import react.semanticui.collections.form._
import react.semanticui.elements.icon.Icon
import react.semanticui.elements.label._
import react.semanticui.sizes._
import react.semanticui.collections.form.Form.FormProps

final case class CoordinatesForm(
  target:           TargetResult,
  searchAndGo:      SearchCallback => Callback,
  goToRaDec:        Coordinates => Callback,
  onNameChange:     NonEmptyString => Callback
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
    tracking:    SiderealTracking,
    searching:   Boolean,
    searchError: Option[String]
  )

  object State {
    val baseCoordinates =
      State.tracking ^|-> SiderealTracking.baseCoordinates

    val raValue =
      baseCoordinates ^|-> Coordinates.rightAscension

    val decValue =
      baseCoordinates ^|-> Coordinates.declination
  }

  def initialState(p: Props): State = {
    val r = targetPropsL.get(p.target)
    Function.tupled(State.apply _)((r._1, r._2, false, none))
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
              _ => searchComplete *> $.setStateL(State.searchError)(s"Search error...".some)
            )

        def iconKeyPress(e: ReactKeyboardEvent): Callback =
          search *> e.stopPropagationCB *> e.preventDefaultCB

        val nameLabel = (state.searchTerm.isEmpty, state.searchError) match {
          case (_, Some(m)) => Label(clazz = ExploreStyles.ErrorLabel)(m)
          case (true, _)    => Label(clazz = ExploreStyles.ErrorLabel)("Cannot be empty")
          case _            => Label("Name")
        }

        val submitForm: Form.OnSubmitE =
          (e: Form.ReactFormEvent, _: FormProps) => e.preventDefaultCB *> search

        Form(size = Small, onSubmitE = submitForm)(
          ExploreStyles.Grid,
          ExploreStyles.Compact,
          ExploreStyles.CoordinatesForm,
          FormInputEV(
            id = "search",
            value = stateView.zoom(State.searchTerm),
            label = nameLabel,
            focus = true,
            loading = state.searching,
            error = state.searchTerm.isEmpty,
            onTextChange = (u: String) =>
              $.setStateL(State.searchTerm)(u) *> $.setStateL(State.searchError)(none),
            onBlur = (u: ValidatedNec[String, String]) =>
              u.toOption
                .flatMap(
                  refineV[NonEmpty](_).toOption
                    .map(props.onNameChange(_))
                )
                .getOrEmpty,
            disabled = state.searching,
            icon = Icons.Search
              .link(true)
              .clazz(ExploreStyles.ButtonIcon)(
                ^.tabIndex := 0,
                ^.onKeyPress ==> iconKeyPress,
                ^.onMouseUp --> search,
                ^.onTouchEnd --> search
              )
          ).withMods(^.autoComplete := "off", ^.placeholder := "Name"),
          // We need this hidden control to submit when pressing enter
          <.input(^.`type` := "submit", ^.hidden := true),
          <.div(
            ExploreStyles.FlexContainer,
            ExploreStyles.TargetRaDecMinWidth,
            FormInputEV(
              id = "ra",
              value = stateView.zoom(State.raValue),
              validFormat = ValidFormatInput.fromFormat(RightAscension.fromStringHMS),
              label = "RA",
              clazz = ExploreStyles.FlexGrow(1) |+| ExploreStyles.TargetRaDecMinWidth,
              disabled = true
            ),
            FormInputEV(
              id = "dec",
              value = stateView.zoom(State.decValue),
              validFormat = ValidFormatInput.fromFormat(Declination.fromStringSignedDMS),
              label = "Dec",
              clazz = ExploreStyles.FlexGrow(1) |+| ExploreStyles.TargetRaDecMinWidth,
              disabled = true
            ),
            FormButton(
              size = Small,
              icon = true,
              label = "Go To",
              onClick = props.goToRaDec(State.baseCoordinates.get(state))
            )(
              Icon("angle right"),
              ExploreStyles.HideLabel
            ).when(false)
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
