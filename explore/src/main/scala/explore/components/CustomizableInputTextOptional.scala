// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.Eq
import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.syntax.ui.*
import japgolly.scalajs.react.ScalaFnComponent
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.validation.InputValidFormat
import lucuma.react.common.ReactFnProps
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

import scalajs.js.JSConverters.*

/**
 * An input text component that allows the user to enter a value. But in this case None represents a
 * default value. If the current value is not None, then an addon icon with a tooltip is displayed.
 * Clicking on the icon sets the value to None (which represents the default value). Explicitly
 * entering the default value will also set it to None.
 */
final case class CustomizableInputTextOptional[A: Eq](
  id:            NonEmptyString,
  value:         View[Option[A]],
  validFormat:   InputValidFormat[Option[A]],
  changeAuditor: ChangeAuditor,
  label:         TagMod,
  defaultValue:  A,
  units:         Option[String],
  disabled:      Boolean
)(using val eq: Eq[A])
    extends ReactFnProps(CustomizableInputTextOptional.component)

object CustomizableInputTextOptional:
  private def buildComponent[A] = ScalaFnComponent[CustomizableInputTextOptional[A]](props =>
    import props.given

    val originalText = props.validFormat.reverseGet(props.defaultValue.some)
    val customAddon  =
      props.value.get.map(_ => CustomizedGroupAddon(originalText, props.value.set(none)): TagMod)

    FormInputTextView(
      id = props.id,
      value = props.value.withDefault(props.defaultValue),
      label = props.label,
      units = props.units.orUndefined,
      postAddons = customAddon.toList,
      validFormat = props.validFormat,
      changeAuditor = props.changeAuditor,
      disabled = props.disabled
    ).clearable(^.autoComplete.off)
  )

  private val component = buildComponent[Any]
