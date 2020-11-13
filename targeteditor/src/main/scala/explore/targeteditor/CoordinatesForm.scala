// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import explore.AppCtx
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.ModelOptics._
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
import lucuma.ui.optics.ChangeAuditor
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
  name:             View[NonEmptyString],
  tracking:         View[SiderealTracking],
  searching:        View[Boolean],
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
    NonEmptyString
      .from(searchTerm)
      .toOption
      .map(s => before >> searchAndGo(SearchCallback(s, onComplete, onError)))
      .getOrEmpty
}

object CoordinatesForm {
  type Props = CoordinatesForm

  @Lenses
  final case class State(
    initialName:   NonEmptyString,
    searchTerm:    String, // Shadow tracking of name input for the case of submit without blur (enter key)
    searchEnabled: Boolean,
    searchError:   Option[NonEmptyString]
  )

  implicit val stateReuse                     = Reusability.derive[State]
  implicit val propsReuse: Reusability[Props] =
    Reusability.by(x => (x.name, x.tracking, x.searching))

  class Backend($ : BackendScope[Props, State]) {

    def render(props: Props, state: State) =
      AppCtx.withCtx { implicit appCtx =>
        val searchComplete: IO[Unit] = props.searching.set(false)

        val search: Callback =
          props
            .submit(
              state.searchTerm,
              props.searching.set(true).runAsyncCB,
              t =>
                searchComplete.runAsyncCB *> ($.setStateL(State.searchError)(
                  NonEmptyString.unsafeFrom(s"'${abbreviate(state.searchTerm, 10)}' not found").some
                )).when_(t.isEmpty),
              _ =>
                searchComplete.runAsyncCB *> $.setStateL(State.searchError)(
                  NonEmptyString("Search error...").some
                )
            )

        def iconKeyPress(e: ReactKeyboardEvent): Callback =
          search *> e.stopPropagationCB *> e.preventDefaultCB

        val submitForm: Form.OnSubmitE =
          (e: Form.ReactFormEvent, _: FormProps) =>
            e.preventDefaultCB *> search.when(state.searchEnabled).void

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
          ExploreStyles.CoordinatesForm,
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
            onTextChange =
              v => $.setStateL(State.searchTerm)(v) >> $.setStateL(State.searchError)(none),
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
              value = props.tracking.zoom(properMotionRA),
              validFormat = ValidFormatInput.fromFormat(RightAscension.fromStringHMS),
              changeAuditor = ChangeAuditor.rightAscension,
              label = "RA",
              clazz = ExploreStyles.FlexGrow(1) |+| ExploreStyles.TargetRaDecMinWidth
            ),
            FormInputEV(
              id = "dec",
              value = props.tracking.zoom(properMotionDec),
              validFormat = ValidFormatInput.fromFormat(Declination.fromStringSignedDMS),
              changeAuditor = ChangeAuditor.declination,
              label = "Dec",
              clazz = ExploreStyles.FlexGrow(1) |+| ExploreStyles.TargetRaDecMinWidth
            ),
            FormButton(
              size = Small,
              icon = true,
              label = "Go To",
              onClick = props.goToRaDec(props.tracking.get.baseCoordinates)
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
      .getDerivedStateFromPropsAndState[State] { (props, stateOpt) =>
        val propsName = props.name.get
        stateOpt match {
          case Some(state) if state.initialName === propsName =>
            state
          case _                                              => // Initialize or reset.
            State(
              initialName = propsName,
              searchTerm = propsName.value,
              searchEnabled = true,
              searchError = stateOpt.flatMap(_.searchError)
            )
        }
      }
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .build

}
