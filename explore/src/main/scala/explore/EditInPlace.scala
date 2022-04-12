// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.syntax.all._
import crystal.react.ReuseView
import crystal.react.reuse._
import eu.timepit.refined.types.string.NonEmptyString
import explore.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.reusability._
import react.common.ReactFnProps
import react.common.style.Css
import react.semanticui.collections.form.FormInput
import react.semanticui.elements.button.Button
import react.semanticui.sizes._

final case class EditInPlace(
  value:             Option[NonEmptyString],
  mod:               Option[NonEmptyString] ==> Callback,
  textClass:         Css = Css.Empty,
  addButtonLabel:    Reuse[VdomNode] = ("Add": VdomNode).reuseAlways,
  addButtonClass:    Css = Css.Empty,
  editButtonClass:   Css = Css.Empty,
  deleteButtonClass: Css = Css.Empty
) extends ReactFnProps[EditInPlace](EditInPlace.component)

object EditInPlace {
  type Props = EditInPlace

  def fromView(
    value:             ReuseView[Option[NonEmptyString]],
    textClass:         Css = Css.Empty,
    addButtonLabel:    Reuse[VdomNode] = ("Add": VdomNode).reuseAlways,
    addButtonClass:    Css = Css.Empty,
    editButtonClass:   Css = Css.Empty,
    deleteButtonClass: Css = Css.Empty
  ): EditInPlace =
    EditInPlace(
      value.get,
      value.map(_.set),
      textClass,
      addButtonLabel,
      addButtonClass,
      editButtonClass,
      deleteButtonClass
    )

  private implicit val reuseProps: Reusability[Props] = Reusability.derive

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateWithReuse(false)                                            // editing
      .useStateWithReuseBy((props, _) => props.value.map(_.value).orEmpty) // displayValue
      .renderWithReuse { (props, editing, displayValue) =>
        def editCB(e: ReactMouseEvent): Callback =
          e.stopPropagationCB >> e.preventDefaultCB >> editing.setState(true)

        val submitCB: Callback =
          props.mod(NonEmptyString.from(displayValue.value).toOption) >>
            editing.setState(false)

        if (editing.value)
          FormInput(
            value = displayValue.value,
            onChangeE = (e: ReactEventFromInput) => displayValue.setState(e.target.value).value,
            size = Mini
          )(
            ^.onBlur --> submitCB,
            ^.onKeyUp ==> (e => if (e.key == "Enter") submitCB else Callback.empty),
            ^.onClick ==> (e => e.stopPropagationCB >> e.preventDefaultCB)
          )
        else
          props.value.fold[VdomNode](
            Button(
              size = Mini,
              compact = true,
              clazz = props.addButtonClass,
              onClickE = (e: ReactMouseEvent, _: Button.ButtonProps) => editCB(e)
            )(
              props.addButtonLabel
            )
          )(
            <.span(
              props.textClass,
              _,
              Button(
                size = Mini,
                compact = true,
                clazz = props.editButtonClass,
                onClickE = (e: ReactMouseEvent, _: Button.ButtonProps) => editCB(e)
              )(
                Icons.Edit
              ),
              Button(
                size = Mini,
                compact = true,
                clazz = props.deleteButtonClass,
                onClickE = (e: ReactMouseEvent, _: Button.ButtonProps) =>
                  e.stopPropagationCB >> e.preventDefaultCB >>
                    props.mod(none) >> displayValue.setState("")
              )(
                Icons.Trash
              )
            )
          )
      }

}
