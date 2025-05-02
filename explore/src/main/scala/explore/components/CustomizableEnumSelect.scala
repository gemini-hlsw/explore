// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.Help
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.syntax.all.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.PrimeStyles
import lucuma.ui.primereact.FormEnumDropdownView
import lucuma.ui.primereact.FormLabel
import lucuma.ui.primereact.LucumaPrimeStyles
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

/**
 * An enum select component that allows the user to select an enum value from a list of options. But
 * it has the concept of a default value that can be reverted to. If the current value is not the
 * same as the default value, then an addon icon with a tooltip is displayed. Clicking on the icon
 * reverts the value to the default value.
 */
final case class CustomizableEnumSelect[A: Enumerated: Display](
  id:           NonEmptyString,
  view:         View[A],
  defaultValue: A,
  disabled:     Boolean,
  exclude:      Set[A] = Set.empty[A],
  label:        Option[String] = None,
  helpId:       Option[Help.Id] = None
)(using val display: Display[A], val enumerated: Enumerated[A])
    extends ReactFnProps(CustomizableEnumSelect.component)

object CustomizableEnumSelect:
  private def buildComponent[A] = ScalaFnComponent[CustomizableEnumSelect[A]](props =>
    import props.given

    val originalText = props.defaultValue.shortName

    React.Fragment(
      props.label.map(label => FormLabel(htmlFor = props.id)(label, props.helpId.map(HelpIcon(_)))),
      <.span(
        LucumaPrimeStyles.FormField,
        PrimeStyles.InputGroup,
        FormEnumDropdownView(
          id = props.id,
          value = props.view,
          exclude = props.exclude,
          disabled = props.disabled
        ),
        <.span(PrimeStyles.InputGroupAddon,
               CustomizedGroupAddon(originalText, props.view.set(props.defaultValue))
        ).when(props.view.get =!= props.defaultValue)
      )
    )
  )

  private val component = buildComponent[Any]
