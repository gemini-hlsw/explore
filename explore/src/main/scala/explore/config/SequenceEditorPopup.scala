// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import crystal.react.reuse._
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.ui.reusability._
import react.common.ReactFnProps
import react.semanticui.elements.button.Button
import react.semanticui.modules.modal._
import react.semanticui.shorthand._
import react.semanticui.sizes._

final case class SequenceEditorPopup(
  obsId:            Observation.Id,
  title:            String,
  subtitle:         Option[NonEmptyString],
  trigger:          Reuse[Button]
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[SequenceEditorPopup](SequenceEditorPopup.component)

object SequenceEditorPopup {
  type Props = SequenceEditorPopup

  private implicit val propsReuse: Reusability[Props] = Reusability.derive

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      // isOpen
      .useState(false)
      .renderWithReuse((props, isOpen) =>
        React.Fragment(
          props.trigger.value(^.onClick --> isOpen.setState(true)),
          Modal(
            actions = List(
              Button(size = Small, icon = true)(
                Icons.Close,
                "Close"
              )(^.tpe := "button", ^.key := "input-cancel")
            ),
            centered = false, // Works better on iOS
            open = isOpen.value,
            closeIcon = Icons.Close.clazz(ExploreStyles.ModalCloseButton),
            dimmer = Dimmer.Blurring,
            size = ModalSize.Small,
            onClose = isOpen.setState(false),
            header = ModalHeader(
              <.div(s"${props.obsId}: ${props.title}"),
              props.subtitle.map(subtitle => <.div(ExploreStyles.SequenceObsSutitle, subtitle))
            ),
            content = ModalContent(
              GeneratedSequenceViewer(props.obsId)
            )
          )
        )
      )
}
