// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.NewBoolean
import lucuma.react.common.ReactFnProps
import lucuma.react.common.style.Css
import lucuma.react.fa.FontAwesomeIcon
import lucuma.react.floatingui.syntax.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.InputText
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.given
import org.scalajs.dom

import scalajs.js.JSConverters.*

case class EditableLabel(
  value:                Option[NonEmptyString],
  mod:                  Option[NonEmptyString] => Callback,
  forceEditing:         Boolean = false,
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
  discardButtonTooltip: Option[String] = None,
  readonly:             Boolean = false
) extends ReactFnProps(EditableLabel.component)

object EditableLabel:
  private type Props = EditableLabel

  def fromView(
    value:                View[Option[NonEmptyString]],
    forceEditing:         Boolean = false,
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
    discardButtonTooltip: Option[String] = None,
    readonly:             Boolean = false
  ): EditableLabel =
    EditableLabel(
      value.get,
      value.set,
      forceEditing,
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
      discardButtonTooltip,
      readonly
    )

  private object Editing extends NewBoolean:
    inline def InEdition = True; inline def NotEditing = False

  import Editing.*

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateBy(props => Editing(props.forceEditing)) // editing
      .useEffectWithDepsBy((props, _) => props.forceEditing): (_, editing) =>
        forceEditing => if forceEditing then editing.setState(InEdition) else Callback.empty
      .useState("")                                     // displayValue
      .useId
      .render: (props, editing, displayValue, id) =>
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
              id = id,
              value = displayValue.value,
              onChange = (e: ReactEventFromInput) => displayValue.setState(e.target.value),
              clazz = props.inputClass
            ).mini.withMods(
              ^.onKeyUp ==> (e =>
                // This is odd, it works fine in demo but not in explore
                if (e.key === " ") displayValue.modState(_ + " ")
                else if (e.key === "Enter") submitCB
                else if (e.key === "Escape") editing.setState(NotEditing)
                else Callback.empty
              ),
              ^.onClick ==> (e => e.stopPropagationCB >> e.preventDefaultCB),
              ^.untypedRef(focus)
            ),
            props.okButtonTooltip.fold(acceptButton)(tt => <.span(acceptButton).withTooltip(tt)),
            props.discardButtonTooltip
              .fold(discardButton)(tt => <.span(discardButton).withTooltip(tt))
          )
        else
          props.value.fold[VdomNode](
            if (props.readonly)
              EmptyVdom
            else
              Button(
                severity = Button.Severity.Secondary,
                clazz = props.addButtonClass,
                onClickE = editCB
              ).mini
                .compact(props.addButtonLabel)
          )(text =>
            <.div(^.width := "100%", ^.display.flex)(
              <.span(
                props.textClass,
                ^.onClick ==> (v => editCB(v).whenA(props.editOnClick && !props.readonly)),
                text
              ),
              leftButton.unless(props.readonly),
              rightButton.unless(props.readonly)
            )
          )
