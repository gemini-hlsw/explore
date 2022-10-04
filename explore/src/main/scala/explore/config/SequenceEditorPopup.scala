// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import crystal.Pot
import crystal.implicits.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Observation
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.semanticui.elements.button.Button
import react.semanticui.modules.modal.*
import react.semanticui.shorthand.*
import react.semanticui.sizes.*

case class SequenceEditorPopup(
  obsId:          Observation.Id,
  title:          String,
  subtitle:       Option[NonEmptyString],
  dithersControl: Callback => VdomElement,
  offsetsControl: Callback => VdomElement,
  trigger:        VdomElement
) extends ReactFnProps(SequenceEditorPopup.component)

object SequenceEditorPopup:
  private type Props = SequenceEditorPopup

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useState(false)        // isOpen
      .useStateView(().ready) // changed - Indicates whether to display sequence or pending.
      .render { (props, isOpen, changed) =>
        React.Fragment(
          <.span(^.onClick --> isOpen.setState(true), props.trigger),
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
            header = ModalHeader(content =
              React.Fragment(
                <.div(s"${props.obsId}: ${props.title}"),
                props.subtitle.map(subtitle => <.div(ExploreStyles.SequenceObsSutitle, subtitle))
              )
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
