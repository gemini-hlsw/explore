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
import react.semanticui.collections.form.Form.FormProps
import react.semanticui.collections.form._
import react.semanticui.elements.icon.Icon
import react.semanticui.elements.label.LabelPointing
import react.semanticui.sizes._

import scalajs.js.JSConverters._

final case class CoordinatesForm(
  target:           TargetResult,
  searching:        View[Boolean],
  searchAndGo:      SearchCallback => Callback,
  goToRaDec:        Coordinates => Callback,
  onNameChange:     NonEmptyString => IO[Unit]
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
    searchTerm:    NonEmptyString,
    tracking:      SiderealTracking,
    searchEnabled: Boolean,
    searchError:   Option[NonEmptyString]
  )

  object State {
    val baseCoordinates =
      State.tracking ^|-> SiderealTracking.baseCoordinates

    val raValue =
      baseCoordinates ^|-> Coordinates.rightAscension

    val decValue =
      baseCoordinates ^|-> Coordinates.declination
  }

  implicit val stateReuse                     = Reusability.derive[State]
  implicit val propsReuse: Reusability[Props] = Reusability.by(x => (x.target, x.searching))

  class Backend($ : BackendScope[Props, State]) {

    def render(props: Props, state: State) =
      AppCtx.withCtx { implicit appCtx =>
        val stateView = ViewF.fromState[IO]($)

        val searchComplete: IO[Unit] = props.searching.set(false)

        val search: Callback =
          props
            .submit(
              state.searchTerm,
              props.searching.set(true).runInCB,
              t =>
                searchComplete.runInCB *> ($.setStateL(State.searchError)(
                  NonEmptyString.unsafeFrom(s"'${abbreviate(state.searchTerm, 10)}' not found").some
                )).when_(t.isEmpty),
              _ =>
                searchComplete.runInCB *> $.setStateL(State.searchError)(
                  NonEmptyString("Search error...").some
                )
            )

        def iconKeyPress(e: ReactKeyboardEvent): Callback =
          search *> e.stopPropagationCB *> e.preventDefaultCB

        val submitForm: Form.OnSubmitE =
          (e: Form.ReactFormEvent, _: FormProps) => e.preventDefaultCB *> search

        val searchIcon                 =
          (if (state.searchEnabled)
             Icons.Search
               .link(true)(
                 ^.onKeyPress ==> iconKeyPress,
                 ^.onClick --> search
               )
           else
             Icons.Ban)
            .clazz(ExploreStyles.ButtonIcon)(^.tabIndex := 0)

        Form(size = Small, onSubmitE = submitForm)(
          ExploreStyles.Grid,
          ExploreStyles.Compact,
          ExploreStyles.CoordinatesForm,
          FormInputEV(
            id = "search",
            value = stateView.zoom(State.searchTerm).withOnMod(props.onNameChange),
            validFormat = ValidFormatInput.nonEmptyValidFormat,
            label = "Name",
            error = state.searchError.orUndefined,
            loading = props.searching.get,
            disabled = props.searching.get,
            errorClazz = ExploreStyles.InputErrorTooltip,
            errorPointing = LabelPointing.Below,
            onTextChange = _ => $.setStateL(State.searchError)(none),
            onValidChange = valid => $.setStateL(State.searchEnabled)(valid),
            icon = searchIcon
          ).withMods(^.autoFocus := true, ^.placeholder := "Name"),
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
      .getDerivedStateFromPropsAndState[State] { (props, stateOpt) =>
        val r = targetPropsL.get(props.target)
        // Force new value from props if the prop changes (or we are initializing).
        stateOpt match {
          case Some(state) if state.searchTerm === r._1 && state.tracking === r._2 => state
          case _                                                                   =>
            State(
              searchTerm = r._1,
              tracking = r._2,
              searchEnabled = true,
              searchError = stateOpt.flatMap(_.searchError)
            )
        }
      }
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .build

}
