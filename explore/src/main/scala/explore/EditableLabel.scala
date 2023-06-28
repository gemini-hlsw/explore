// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.NewType
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.given
import org.scalajs.dom
import react.common.ReactFnProps
import react.common.style.Css
import react.fa.FontAwesomeIcon
import react.floatingui.syntax.*
import react.primereact.Button
import react.primereact.InputText

import scalajs.js.JSConverters.*

case class EditableLabel(
  value:                Option[NonEmptyString],
  mod:                  Option[NonEmptyString] => Callback,
  editOnClick:          Boolean = false,
  textClass:            Css = Css.Empty,
  inputClass:           Css = Css.Empty,
  addButtonLabel:       VdomNode = "Add": VdomNode,
  addButtonClass:       Css = Css.Empty,
  leftButtonClass:      Css = Css.Empty,
  leftButtonTooltip:    Option[String] = None,
  rightButtonClass:     Css = Css.Empty,
  rightButtonIcon:      FontAwesomeIcon = Icons.Eraser,
  rightButtonTooltip:   Option[String] = None,
  okButtonTooltip:      Option[String] = None,
  discardButtonTooltip: Option[String] = None
) extends ReactFnProps(EditableLabel.component)

object EditableLabel {
  type Props = EditableLabel

  def fromView(
    value:                View[Option[NonEmptyString]],
    editOnClick:          Boolean = false,
    textClass:            Css = Css.Empty,
    inputClass:           Css = Css.Empty,
    addButtonLabel:       VdomNode = "Add": VdomNode,
    addButtonClass:       Css = Css.Empty,
    editButtonClass:      Css = Css.Empty,
    editButtonTooltip:    Option[String] = None,
    deleteButtonClass:    Css = Css.Empty,
    deleteButtonIcon:     FontAwesomeIcon = Icons.Eraser,
    deleteButtonTooltip:  Option[String] = None,
    okButtonTooltip:      Option[String] = None,
    discardButtonTooltip: Option[String] = None
  ): EditableLabel =
    EditableLabel(
      value.get,
      value.set,
      editOnClick,
      textClass,
      inputClass,
      addButtonLabel,
      addButtonClass,
      editButtonClass,
      editButtonTooltip,
      deleteButtonClass,
      deleteButtonIcon,
      deleteButtonTooltip,
      okButtonTooltip,
      discardButtonTooltip
    )

  type Editing = Editing.Type
  object Editing extends NewType[Boolean]:
    inline def NotEditing = Editing(false)
    inline def InEdition  = Editing(true)

  import Editing.*

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useState(NotEditing) // editing
      .useState("")         // displayValue
      .render { (props, editing, displayValue) =>
        def editCB(e: ReactMouseEvent): Callback =
          e.stopPropagationCB >> e.preventDefaultCB >>
            displayValue.setState(props.value.map(_.value).orEmpty) >>
            editing.setState(InEdition)

        val submitCB: Callback =
          props.mod(NonEmptyString.from(displayValue.value).toOption) >>
            editing.setState(NotEditing)

        def focus(node: dom.Node | Null): Unit =
          Option(node.asInstanceOf[dom.html.Element]).foreach(_.focus())

        val leftButton: VdomNode = Button(
          icon = Icons.Edit,
          text = true,
          severity = Button.Severity.Secondary,
          clazz = props.leftButtonClass,
          onClickE = editCB,
          tooltip = props.leftButtonTooltip.orUndefined
        ).mini.compact

        val rightButton: VdomNode = Button(
          icon = props.rightButtonIcon,
          text = true,
          severity = Button.Severity.Secondary,
          clazz = props.rightButtonClass,
          onClickE = e => e.stopPropagationCB >> e.preventDefaultCB >> props.mod(none),
          tooltip = props.rightButtonTooltip.orUndefined
        ).mini.compact

        val acceptButton: VdomNode = Button(
          icon = Icons.Checkmark,
          text = true,
          severity = Button.Severity.Secondary,
          clazz = props.leftButtonClass,
          onClickE = e => e.stopPropagationCB >> e.preventDefaultCB >> submitCB
        ).mini.compact

        val discardButton: VdomNode = Button(
          icon = Icons.Close,
          text = true,
          severity = Button.Severity.Secondary,
          clazz = props.rightButtonClass,
          onClickE = e => e.stopPropagationCB >> e.preventDefaultCB >> editing.setState(NotEditing)
        ).mini.compact

        if (editing.value.value)
          <.div(^.width := "100%", ^.display.flex)(
            InputText(
              id = "editable-label-input", // won't necesessarily be unique...
              value = displayValue.value,
              onChange = (e: ReactEventFromInput) => displayValue.setState(e.target.value),
              clazz = props.inputClass
            ).mini.withMods(
              ^.onKeyUp ==> (e =>
                if (e.key === "Enter") submitCB
                else if (e.key === "Escape") editing.setState(NotEditing)
                else Callback.empty
              ),
              ^.onClick ==> (e => e.stopPropagationCB >> e.preventDefaultCB),
              ^.untypedRef(focus)
            ),
            props.okButtonTooltip.fold(acceptButton)(tt => <.span(acceptButton).withTooltip(tt)),
            props.discardButtonTooltip.fold(discardButton)(tt =>
              <.span(discardButton).withTooltip(tt)
            )
          )
        else
          props.value.fold[VdomNode](
            Button(
              severity = Button.Severity.Secondary,
              clazz = props.addButtonClass,
              onClickE = editCB
            ).mini.compact(props.addButtonLabel)
          )(text =>
            <.div(^.width := "100%", ^.display.flex)(
              <.span(
                props.textClass,
                ^.onClick ==> (v => editCB(v).whenA(props.editOnClick)),
                text
              ),
              leftButton,
              rightButton
            )
          )
      }

}
