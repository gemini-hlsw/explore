// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import crystal.react.hooks.*
import crystal.react.reuse.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.refined.*
import lucuma.ui.forms.FormInputEV
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.Css
import react.common.ReactFnProps
import react.semanticui.elements.button.Button
import react.semanticui.modules.modal.*
import react.semanticui.sizes.Small

import scala.language.implicitConversions

/**
 * Generic component to accept user input
 */
case class InputModal(
  title:        String,
  initialValue: Option[NonEmptyString],
  label:        String,
  placeholder:  String,
  okLabel:      String,
  trigger:      Reuse[VdomNode],
  onComplete:   NonEmptyString ==> Callback
) extends ReactFnProps[InputModal](InputModal.component)

object InputModal {
  protected type Props = InputModal

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateViewBy(_.initialValue.fold("")(_.value)) // inputValue
      .render { (props, inputValue) =>
        val cleanInput = inputValue.set("")

        Modal(
          as = <.form,      // This lets us sumbit on enter
          actions = List(
            Button(
              size = Small,
              primary = true,
              positive = true,
              disabled = inputValue.get.isEmpty,
              icon = true,
              onClick = cleanInput *> props.onComplete(
                NonEmptyString.from(inputValue.get).getOrElse("------".refined)
              )
            )(
              Icons.Checkmark,
              props.okLabel
            )(^.key := "input-ok"),
            Button(size = Small, icon = true, negative = true, onClick = cleanInput)(
              Icons.Close,
              "Cancel"
            )(^.key := "input-cancel")
          ),
          centered = false, // Works better on iOS
          trigger = props.trigger.value,
          closeIcon = Icons.Close.withClass(ExploreStyles.ModalCloseButton),
          dimmer = Dimmer.Blurring,
          size = ModalSize.Small,
          onClose = cleanInput,
          header = ModalHeader(content = props.title),
          content = ModalContent(
            FormInputEV(id = "name".refined, value = inputValue, label = props.label)
              .withMods(^.placeholder := props.placeholder, ^.autoFocus := true)
          )
        )
      }
}
