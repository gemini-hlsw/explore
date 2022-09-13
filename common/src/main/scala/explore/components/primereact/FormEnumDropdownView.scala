// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.primereact

import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.vdom.html_<^.*
import japgolly.scalajs.react.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import react.common.*
import react.primereact.PrimeStyles
import reactST.primereact.selectitemMod.SelectItem

import scalajs.js
import scalajs.js.JSConverters.*
import lucuma.core.enums.Half.A

final case class FormEnumDropdownView[A](
  id:              NonEmptyString,
  value:           View[A],
  label:           js.UndefOr[String] = js.undefined,
  exclude:         Set[A] = Set.empty[A],
  className:       js.UndefOr[String] = js.undefined,
  clazz:           js.UndefOr[Css] = js.undefined,
  filter:          js.UndefOr[Boolean] = js.undefined,
  showFilterClear: js.UndefOr[Boolean] = js.undefined,
  disabled:        js.UndefOr[Boolean] = js.undefined,
  placeholder:     js.UndefOr[String] = js.undefined
)(using
  val enumerated:  Enumerated[A],
  val display:     Display[A]
) extends ReactFnProps[FormEnumDropdownView[Any]](FormEnumDropdownView.component)

object FormEnumDropdownView {
  type AnyF[_]          = Any
  private type Props[A] = FormEnumDropdownView[A]

  private def buildComponent[A] = ScalaFnComponent[FormEnumDropdownView[A]] { props =>
    import props.given

    React.Fragment(
      props.label.map(l => FormLabel(htmlFor = props.id)(l)),
      EnumDropdownView(
        id = props.id,
        value = props.value,
        exclude = props.exclude,
        className = props.className,
        clazz = PrimeStyles.FormField |+| props.clazz.toOption.orEmpty,
        filter = props.filter,
        showFilterClear = props.showFilterClear,
        placeholder = props.placeholder
      )
    )
  }

  protected val component = buildComponent[Any]
}
