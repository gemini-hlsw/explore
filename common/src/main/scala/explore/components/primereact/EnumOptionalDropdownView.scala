// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.primereact

import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import react.common.*
import react.primereact.Dropdown
import react.primereact.SelectItem
import reactST.primereact.components.{Dropdown => CDropdown}

import scalajs.js
import scalajs.js.JSConverters.*

object EnumOptionalDropdownView:
  def apply[A](
    id:              NonEmptyString,
    value:           View[Option[A]],
    exclude:         Set[A] = Set.empty[A],
    className:       js.UndefOr[String] = js.undefined,
    clazz:           js.UndefOr[Css] = js.undefined,
    showClear:       Boolean = true,
    filter:          js.UndefOr[Boolean] = js.undefined,
    showFilterClear: js.UndefOr[Boolean] = js.undefined,
    disabled:        js.UndefOr[Boolean] = js.undefined,
    placeholder:     js.UndefOr[String] = js.undefined
  )(using
    enumerated:      Enumerated[A],
    display:         Display[A]
  ): CDropdown.Builder =
    Dropdown(
      id = id.value,
      value = value.get.orUndefined,
      options = enumerated.all
        .filter(v => !exclude.contains(v))
        .map(e => SelectItem(label = display.shortName(e), value = e)),
      showClear = showClear,
      className = className,
      clazz = clazz,
      filter = filter,
      showFilterClear = showFilterClear,
      placeholder = placeholder,
      disabled = disabled,
      onChange = v => value.set(if (js.isUndefined(v)) none else v.asInstanceOf[A].some)
    )
