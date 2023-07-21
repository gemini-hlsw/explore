// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import _root_.react.common.ReactFnProps
import _root_.react.primereact.Button
import _root_.react.primereact.Dialog
import _root_.react.primereact.DialogPosition
import crystal.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.util.NewType
import lucuma.ui.primereact.LucumaPrimeStyles
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.given

case class SequenceEditorPopup(
  programId:      Program.Id,
  obsId:          Observation.Id,
  title:          String,
  subtitle:       Option[NonEmptyString],
  dithersControl: Callback => VdomElement,
  offsetsControl: Callback => VdomElement,
  trigger:        VdomElement
) extends ReactFnProps(SequenceEditorPopup.component)

object SequenceEditorPopup:
  private type Props = SequenceEditorPopup

  private object IsOpen extends NewType[Boolean]

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateView(IsOpen(false))
      .useStateView(().ready) // changed - Indicates whether to display sequence or pending.
      .render { (props, isOpen, changed) =>
        React.Fragment(
          <.span(^.onClick --> isOpen.set(IsOpen(true)), props.trigger),
          Dialog(
            footer = Button(
              size = Button.Size.Small,
              icon = Icons.Close,
              label = "Close",
              onClick = isOpen.set(IsOpen(false))
            )
              .withMods(^.key := "input-cancel"),
            position = DialogPosition.Top,
            visible = isOpen.get.value,
            clazz = LucumaPrimeStyles.Dialog.Large,
            dismissableMask = true,
            resizable = false,
            onHide = isOpen.set(IsOpen(false)),
            header = React.Fragment(
              <.div(s"${props.obsId}: ${props.title}"),
              props.subtitle.map(subtitle => <.div(ExploreStyles.SequenceObsSutitle, subtitle))
            )
          )(
            <.div(ExploreStyles.SeqGenParametersForm)(
              <.div(LucumaPrimeStyles.FormColumn)(
                props.dithersControl(changed.set(Pot.pending))
              ),
              <.div(LucumaPrimeStyles.FormColumn)(
                props.offsetsControl(changed.set(Pot.pending))
              )
            ),
            GeneratedSequenceViewer(props.programId, props.obsId, changed)
          )
        )
      }
