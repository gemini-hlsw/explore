// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.implicits.*
import explore.syntax.ui.*
import explore.syntax.ui.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.utils.NewType
import org.scalajs.dom
import react.common.ReactFnProps
import react.common.style.Css
import react.fa.FontAwesomeIcon
import react.floatingui.Tooltip
import react.semanticui.elements.button.Button
import react.semanticui.elements.input.Input
import react.semanticui.sizes.*

import scalajs.js.|

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
  object Editing extends NewType[Boolean] {
    val NotEditing = Editing(false)
    val InEdition  = Editing(true)
  }

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
          size = Mini,
          compact = true,
          clazz = props.leftButtonClass,
          onClickE = (e: ReactMouseEvent, _: Button.ButtonProps) => editCB(e)
        )(Icons.Edit)

        val rightButton: VdomNode = Button(
          size = Mini,
          compact = true,
          clazz = props.rightButtonClass,
          onClickE = (e: ReactMouseEvent, _: Button.ButtonProps) =>
            e.stopPropagationCB >> e.preventDefaultCB >> props.mod(none)
        )(props.rightButtonIcon)

        val acceptButton: VdomNode = Button(
          size = Mini,
          compact = true,
          clazz = props.leftButtonClass,
          onClickE = (e: ReactMouseEvent, _: Button.ButtonProps) =>
            e.stopPropagationCB >> e.preventDefaultCB >> submitCB
        )(Icons.Checkmark)

        val discardButton: VdomNode = Button(
          size = Mini,
          compact = true,
          clazz = props.rightButtonClass,
          onClickE = (e: ReactMouseEvent, _: Button.ButtonProps) =>
            e.stopPropagationCB >> e.preventDefaultCB >> editing.setState(NotEditing)
        )(Icons.Close)

        if (editing.value.value)
          <.div(^.width := "100%", ^.display.flex)(
            Input(
              value = displayValue.value,
              onChangeE = (e: ReactEventFromInput) => displayValue.setState(e.target.value),
              size = Mini,
              focus = true,
              clazz = props.inputClass
            )(
              ^.onKeyUp ==> (e =>
                if (e.key === "Enter") submitCB
                else if (e.key === "Escape") editing.setState(NotEditing)
                else Callback.empty
              ),
              ^.onClick ==> (e => e.stopPropagationCB >> e.preventDefaultCB),
              ^.untypedRef(focus)
            ),
            props.okButtonTooltip
              .map(t => Tooltip(trigger = <.span(acceptButton), tooltip = t): VdomNode)
              .getOrElse(acceptButton),
            props.discardButtonTooltip
              .map(t => Tooltip(trigger = <.span(discardButton), tooltip = t): VdomNode)
              .getOrElse(discardButton)
          )
        else
          props.value.fold[VdomNode](
            Button(
              size = Mini,
              compact = true,
              clazz = props.addButtonClass,
              onClickE = (e: ReactMouseEvent, _: Button.ButtonProps) => editCB(e)
            )(props.addButtonLabel)
          )(text =>
            <.div(^.width := "100%", ^.display.flex)(
              <.span(
                props.textClass,
                ^.onClick ==> (v => editCB(v).whenA(props.editOnClick)),
                text
              ),
              props.leftButtonTooltip
                .map(t => Tooltip(trigger = <.span(leftButton), tooltip = t): VdomNode)
                .getOrElse(leftButton),
              props.rightButtonTooltip
                .map(t => Tooltip(trigger = <.span(rightButton), tooltip = t): VdomNode)
                .getOrElse(rightButton)
            )
          )
      }

}
