// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.syntax.all._
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.implicits._
import explore.syntax.ui.*
import explore.syntax.ui.given
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import org.scalajs.dom
import react.common.ReactFnProps
import react.common.style.Css
import react.fa.FontAwesomeIcon
import react.semanticui.elements.button.Button
import react.semanticui.elements.input.Input
import react.semanticui.sizes._

import scalajs.js.|

final case class EditableLabel(
  value:            Option[NonEmptyString],
  mod:              Option[NonEmptyString] => Callback,
  editOnClick:      Boolean = false,
  textClass:        Css = Css.Empty,
  inputClass:       Css = Css.Empty,
  addButtonLabel:   VdomNode = "Add": VdomNode,
  addButtonClass:   Css = Css.Empty,
  leftButtonClass:  Css = Css.Empty,
  rightButtonClass: Css = Css.Empty,
  rightButtonIcon:  FontAwesomeIcon = Icons.Eraser
) extends ReactFnProps[EditableLabel](EditableLabel.component)

object EditableLabel {
  type Props = EditableLabel

  def fromView(
    value:             View[Option[NonEmptyString]],
    editOnClick:       Boolean = false,
    textClass:         Css = Css.Empty,
    inputClass:        Css = Css.Empty,
    addButtonLabel:    VdomNode = "Add": VdomNode,
    addButtonClass:    Css = Css.Empty,
    editButtonClass:   Css = Css.Empty,
    deleteButtonClass: Css = Css.Empty,
    deleteButtonIcon:  FontAwesomeIcon = Icons.Eraser
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
      deleteButtonClass,
      deleteButtonIcon
    )

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useState(false) // editing
      .useState("")    // displayValue
      .render { (props, editing, displayValue) =>
        def editCB(e: ReactMouseEvent): Callback =
          e.stopPropagationCB >> e.preventDefaultCB >>
            displayValue.setState(props.value.map(_.value).orEmpty) >>
            editing.setState(true)

        val submitCB: Callback =
          props.mod(NonEmptyString.from(displayValue.value).toOption) >>
            editing.setState(false)

        def focus(node: dom.Node | Null): Unit =
          Option(node.asInstanceOf[dom.html.Element]).foreach(_.focus())

        if (editing.value)
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
                else if (e.key === "Escape") editing.setState(false)
                else Callback.empty
              ),
              ^.onClick ==> (e => e.stopPropagationCB >> e.preventDefaultCB),
              ^.untypedRef(focus)
            ),
            Button(
              size = Mini,
              compact = true,
              clazz = props.leftButtonClass,
              onClickE = (e: ReactMouseEvent, _: Button.ButtonProps) =>
                e.stopPropagationCB >> e.preventDefaultCB >> submitCB
            )(Icons.Checkmark),
            Button(
              size = Mini,
              compact = true,
              clazz = props.rightButtonClass,
              onClickE = (e: ReactMouseEvent, _: Button.ButtonProps) =>
                e.stopPropagationCB >> e.preventDefaultCB >> editing.setState(false)
            )(Icons.Close)
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
              Button(
                size = Mini,
                compact = true,
                clazz = props.leftButtonClass,
                onClickE = (e: ReactMouseEvent, _: Button.ButtonProps) => editCB(e)
              )(Icons.Edit),
              Button(
                size = Mini,
                compact = true,
                clazz = props.rightButtonClass,
                onClickE = (e: ReactMouseEvent, _: Button.ButtonProps) =>
                  e.stopPropagationCB >> e.preventDefaultCB >> props.mod(none)
              )(props.rightButtonIcon)
            )
          )
      }

}
