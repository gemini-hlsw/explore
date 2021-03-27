// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all._
import crystal.ViewF
import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import explore.AppCtx
import explore.Icons
import explore.components.HelpIcon
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
import react.semanticui.elements.label.Label
import react.semanticui.elements.label.LabelPointing
import react.semanticui.sizes._

import scalajs.js.JSConverters._

final case class SearchForm(
  id:          Target.Id,
  name:        NonEmptyString,
  searching:   View[Set[Target.Id]],
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
    searchTerm:    NonEmptyString,
    searchEnabled: Boolean,
    searchError:   Option[NonEmptyString]
  )

  implicit val stateReuse                     = Reusability.derive[State]
  implicit val propsReuse: Reusability[Props] = Reusability.by(x => (x.id, x.name, x.searching))

  class Backend($ : BackendScope[Props, State]) {

    def render(props: Props, state: State) = AppCtx.using { implicit ctx =>
      val searchComplete: IO[Unit] = props.searching.mod(_ - props.id)

      val search: Callback =
        props
          .submit(
            state.searchTerm,
            $.setStateL(State.searchError)(none) >> props.searching.mod(_ + props.id).runAsyncCB,
            t =>
              searchComplete.runAsyncCB *> ($.setStateL(State.searchError)(
                NonEmptyString
                  .unsafeFrom(s"'${abbreviate(state.searchTerm, 10)}' not found")
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

      val disabled   = props.searching.get.exists(_ === props.id)

      Form(size = Small, onSubmitE = submitForm)(
        ExploreStyles.Grid,
        ExploreStyles.Compact,
        ExploreStyles.SearchForm,
        FormInputEV(
          id = "search",
          value = ViewF.fromState[IO]($).zoom(State.searchTerm),
          validFormat = ValidFormatInput.nonEmptyValidFormat,
          label = Label("Name", HelpIcon("target/main/search-target.md")),
          error = state.searchError.orUndefined,
          loading = disabled,
          disabled = disabled,
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
        stateOpt match {
          case Some(state) if state.initialName === props.name => state
          case _                                               => // Initialize or reset.
            State(
              initialName = props.name,
              searchTerm = props.name,
              searchEnabled = true,
              searchError = stateOpt.flatMap(_.searchError)
            )
        }
      }
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .build

}
