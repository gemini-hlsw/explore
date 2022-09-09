// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.primereact

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import react.common.*
import react.primereact.PrimeStyles

import scalajs.js
import scalajs.js.JSConverters.*

object FormLabel {
  def apply(
    htmlFor: NonEmptyString
  ) = <.label(
    PrimeStyles.FormFieldLabel,
    ^.htmlFor := htmlFor.value
  )
}
