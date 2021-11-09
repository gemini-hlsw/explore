// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import crystal.ViewF
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.ReactMonocle._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.forms.FormInputEV
import lucuma.ui.reusability._
import monocle.Focus
import react.common._
import react.semanticui.elements.button.Button
import react.semanticui.modules.modal._
import react.semanticui.shorthand._
import react.semanticui.sizes.Small

/**
 * Generic component to accept user input
 */
final case class InputModal(
  title:        String,
  initialValue: Option[NonEmptyString],
  label:        String,
  placeholder:  String,
  okLabel:      String,
  trigger:      Reuse[VdomNode],
  onComplete:   NonEmptyString ==> Callback
) extends ReactProps[InputModal](InputModal.component)

object InputModal {
  type Props = InputModal

  final case class State(inputValue: String)

  object State {
    val inputValue = Focus[State](_.inputValue)
  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive
  implicit val stateReuse: Reusability[State] = Reusability.derive

  protected class Backend($ : BackendScope[Props, State]) {
    def render(props: Props, state: State) = {
      val valueView = ViewF.fromState($).zoom(State.inputValue)

      val cleanInput = $.setStateL(State.inputValue)("")

      Modal(
        as = <.form,      // This lets us sumbit on enter
        actions = List(
          Button(
            size = Small,
            primary = true,
            disabled = state.inputValue.isEmpty,
            icon = true,
            onClick = cleanInput *> props.onComplete(
              NonEmptyString.from(state.inputValue).getOrElse("------")
            )
          )(
            Icons.Checkmark,
            props.okLabel
          )(^.key := "input-ok"),
          Button(size = Small, icon = true, onClick = cleanInput)(
            Icons.Close,
            "Cancel"
          )(^.key := "input-cancel")
        ),
        centered = false, // Works betten on iOS
        trigger = props.trigger.value,
        closeIcon = Icons.Close.clazz(ExploreStyles.ModalCloseButton),
        dimmer = Dimmer.Blurring,
        size = ModalSize.Small,
        onClose = cleanInput,
        header = ModalHeader(props.title),
        content = ModalContent(
          FormInputEV(
            id = "name",
            value = valueView,
            label = props.label,
            onTextChange = t => $.setStateL(State.inputValue)(t)
          )
            .withMods(^.placeholder := props.placeholder, ^.autoFocus := true)
        )
      )
    }
  }

  protected val component =
    ScalaComponent
      .builder[Props]
      .initialStateFromProps(p => State(p.initialValue.fold("")(_.value)))
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .build
}
