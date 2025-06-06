// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.Eq
import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.ScalaFnComponent
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.validation.InputValidFormat
import lucuma.react.common.ReactFnProps
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

import scalajs.js.JSConverters.*

/**
 * An input text component that allows the user to enter a value. But it has the concept of a
 * default value that can be reverted to. If the current value is not the same as the default value,
 * then an addon icon with a tooltip is displayed. Clicking on the icon reverts the value to the
 * default value.
 */
final case class CustomizableInputText[A](
  id:            NonEmptyString,
  value:         View[A],
  validFormat:   InputValidFormat[A],
  changeAuditor: ChangeAuditor,
  label:         TagMod,
  defaultValue:  A,
  units:         Option[String],
  disabled:      Boolean
)(using val eq: Eq[A])
    extends ReactFnProps(CustomizableInputText.component)

object CustomizableInputText:
  private def buildComponent[A] = ScalaFnComponent[CustomizableInputText[A]](props =>
    import props.given

    val isCustom                    = props.value.get =!= props.defaultValue
    val customAddon: Option[TagMod] =
      if (isCustom)
        (CustomizedGroupAddon(props.validFormat.reverseGet(props.defaultValue),
                              props.value.set(props.defaultValue)
        ): TagMod).some
      else none

    FormInputTextView(
      id = props.id,
      value = props.value,
      label = props.label,
      units = props.units.orUndefined,
      postAddons = customAddon.toList,
      validFormat = props.validFormat,
      changeAuditor = props.changeAuditor,
      disabled = props.disabled
    ).withMods(^.autoComplete.off)
  )

  private val component = buildComponent[Any]
