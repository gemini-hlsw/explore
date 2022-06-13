// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import crystal.Pot
import crystal.implicits._
import crystal.react.hooks._
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import react.common.ReactFnProps
import react.semanticui.elements.button.Button
import react.semanticui.modules.modal._
import react.semanticui.shorthand._
import react.semanticui.sizes._

final case class SequenceEditorPopup(
  obsId:            Observation.Id,
  title:            String,
  subtitle:         Option[NonEmptyString],
  dithersControl:   Callback => VdomElement,
  offsetsControl:   Callback => VdomElement,
  trigger:          Button
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[SequenceEditorPopup](SequenceEditorPopup.component)

object SequenceEditorPopup {
  type Props = SequenceEditorPopup

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      .useState(false)        // isOpen
      .useStateView(().ready) // changed - Indicates whether to display sequence or pending.
      .render { (props, isOpen, changed) =>
        implicit val ctx = props.ctx

        React.Fragment(
          props.trigger(^.onClick --> isOpen.setState(true)),
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
              <.div(ExploreStyles.SeqGenParametersForm)(
                <.div(ExploreStyles.ExploreForm)(
                  props.dithersControl(changed.set(Pot.pending))
                ),
                <.div(ExploreStyles.ExploreForm)(
                  props.offsetsControl(changed.set(Pot.pending))
                )
              ),
              GeneratedSequenceViewer(props.obsId, changed)
            )
          )
        )
      }
}
