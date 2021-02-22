// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.effect.IO
import crystal.ViewF
import crystal.react.implicits._
import eu.timepit.refined.auto._
import explore.AppCtx
import explore.Icons
import explore.implicits._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.forms.FormInputEV
import monocle.macros.Lenses
import react.common._
import react.semanticui.collections.form.Form
import react.semanticui.elements.button.Button
import react.semanticui.elements.header.Header
import react.semanticui.modules.modal._
import react.semanticui.sizes.Small

final case class InputModal(
  title:        String,
  initialValue: String,
  label:        String,
  placeholder:  String,
  okLabel:      String,
  trigger:      VdomNode,
  onComplete:   String => Callback
) extends ReactProps[InputModal](InputModal.component)

object InputModal {
  type Props = InputModal

  @Lenses
  final case class State(inputValue: String)

  object State {
    val Zero = State("")
  }

  implicit val propsReuse: Reusability[Props] = Reusability.caseClassExcept("trigger", "onComplete")
  implicit val stateReuse: Reusability[State] = Reusability.derive

  protected val component =
    ScalaComponent
      .builder[Props]
      .initialStateFromProps(p => State(p.initialValue))
      .renderPS { ($, props, state) =>
        AppCtx.withCtx { implicit appCtx =>
          val valueView = ViewF.fromState[IO]($).zoom(State.inputValue)

          val cleanInput = $.setStateL(State.inputValue)("")
          Modal(
            actions = List(
              Button(size = Small,
                     primary = true,
                     disabled = state.inputValue.isEmpty,
                     onClick = cleanInput *> props.onComplete(state.inputValue)
              )(
                ^.key := "input-ok",
                Icons.Checkmark,
                props.okLabel
              ),
              Button(size = Small, onClick = cleanInput)(
                ^.key := "input-cancel",
                Icons.Remove,
                "Cancel"
              )
            ),
            trigger = props.trigger,
            closeIcon = Icons.Close,
            dimmer = Dimmer.Blurring,
            size = ModalSize.Small,
            onClose = cleanInput,
            content = ModalContent(
              Header(props.title),
              Form(size = Small)(
                FormInputEV(
                  id = "name",
                  value = valueView,
                  label = props.label,
                  onTextChange = t => $.setStateL(State.inputValue)(t)
                )
                  .withMods(^.placeholder := props.placeholder, ^.autoFocus := true)
              )
            )
          )
        }
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
