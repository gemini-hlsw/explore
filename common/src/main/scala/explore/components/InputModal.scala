// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.effect.IO
import crystal.ViewF
import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import explore.AppCtx
import explore.Icons
import explore.implicits._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.forms.FormInputEV
import lucuma.ui.reusability._
import monocle.macros.Lenses
import react.common._
import react.semanticui.elements.button.Button
import react.semanticui.modules.modal._
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
  trigger:      VdomNode,
  onComplete:   NonEmptyString => Callback
) extends ReactProps[InputModal](InputModal.component)

object InputModal {
  type Props = InputModal

  @Lenses
  final case class State(inputValue: String)

  implicit val propsReuse: Reusability[Props] = Reusability.caseClassExcept("trigger", "onComplete")
  implicit val stateReuse: Reusability[State] = Reusability.derive

  protected val component =
    ScalaComponent
      .builder[Props]
      .initialStateFromProps(p => State(p.initialValue.fold("")(_.value)))
      .renderPS { ($, props, state) =>
        AppCtx.using { implicit appCtx =>
          val valueView = ViewF.fromState[IO]($).zoom(State.inputValue)

          val cleanInput = $.setStateL(State.inputValue)("")

          Modal(
            as = <.form,      // This lets us sumbit on enter
            actions = List(
              Button(
                size = Small,
                primary = true,
                disabled = state.inputValue.isEmpty,
                onClick = cleanInput *> props.onComplete(
                  NonEmptyString.from(state.inputValue).getOrElse("------")
                )
              )(
                Icons.Checkmark,
                props.okLabel
              )(^.key := "input-ok"),
              Button(size = Small, onClick = cleanInput)(
                Icons.Remove,
                "Cancel"
              )(^.key := "input-cancel")
            ),
            centered = false, //Works betten on iOS
            trigger = props.trigger,
            closeIcon = Icons.Close,
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
      .configure(Reusability.shouldComponentUpdate)
      .build
}
