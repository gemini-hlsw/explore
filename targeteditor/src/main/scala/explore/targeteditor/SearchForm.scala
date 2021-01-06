// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.implicits._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Target
import lucuma.ui.forms._
import lucuma.ui.optics.ValidFormatInput
import lucuma.ui.reusability._
import lucuma.ui.utils.abbreviate
import monocle.macros.Lenses
import react.common._
import react.semanticui.collections.form.Form.FormProps
import react.semanticui.collections.form._
import react.semanticui.elements.label.LabelPointing
import react.semanticui.sizes._

import scalajs.js.JSConverters._

final case class SearchForm(
  name:        View[NonEmptyString],
  searching:   View[Boolean],
  searchAndGo: SearchCallback => Callback
) extends ReactProps[SearchForm](SearchForm.component) {
  def submit(
    searchTerm: String,
    before:     Callback,
    onComplete: Option[Target] => Callback,
    onError:    Throwable => Callback
  ): Callback =
    NonEmptyString
      .from(searchTerm)
      .toOption
      .map(s => before >> searchAndGo(SearchCallback(s, onComplete, onError)))
      .getOrEmpty
}

object SearchForm {
  type Props = SearchForm

  @Lenses
  final case class State(
    initialName:   NonEmptyString,
    searchEnabled: Boolean,
    searchError:   Option[NonEmptyString]
  )

  implicit val stateReuse                     = Reusability.derive[State]
  implicit val propsReuse: Reusability[Props] = Reusability.by(x => (x.name, x.searching))

  class Backend($ : BackendScope[Props, State]) {

    def render(props: Props, state: State) = {
      val searchComplete: IO[Unit] = props.searching.set(false)

      val search: Callback =
        props
          .submit(
            props.name.get,
            $.setStateL(State.searchError)(none) >> props.searching.set(true).runAsyncCB,
            t =>
              searchComplete.runAsyncCB *> ($.setStateL(State.searchError)(
                NonEmptyString
                  .unsafeFrom(s"'${abbreviate(props.name.get, 10)}' not found")
                  .some
              )).when_(t.isEmpty),
            _ =>
              searchComplete.runAsyncCB *> $.setStateL(State.searchError)(
                NonEmptyString("Search error...").some
              )
          )

      def iconKeyPress(e: ReactKeyboardEvent): Callback =
        search *> e.stopPropagationCB *> e.preventDefaultCB

      def submitForm: Form.OnSubmitE =
        (
          e: Form.ReactFormEvent,
          _: FormProps
        ) => e.preventDefaultCB *> search.when(state.searchEnabled).void

      val searchIcon =
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
        ExploreStyles.SearchForm,
        FormInputEV(
          id = "search",
          value = props.name,
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
        <.input(^.`type` := "submit", ^.hidden := true)
      )
    }
  }

  val component =
    ScalaComponent
      .builder[Props]
      .getDerivedStateFromPropsAndState[State] { (props, stateOpt) =>
        val propsName = props.name.get
        stateOpt match {
          case Some(state) if state.initialName === propsName => state
          case _                                              => // Initialize or reset.
            State(
              initialName = propsName,
              searchEnabled = true,
              searchError = stateOpt.flatMap(_.searchError)
            )
        }
      }
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .build

}
