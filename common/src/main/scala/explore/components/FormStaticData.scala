// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.TagMod
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps

/**
 * A control similar to a readonly FormInput styled as `static-data`, except that this control will
 * be only as wide as it needs to be.
 */

case class FormStaticData(
  id:        String,
  value:     TagMod,
  label:     String,
  modifiers: Seq[TagMod] = Seq.empty
) extends ReactFnProps[FormStaticData](FormStaticData.component) {
  def apply(mods: TagMod*): FormStaticData = copy(modifiers = modifiers ++ mods)
}

object FormStaticData {
  type Props = FormStaticData

  val component =
    ScalaFnComponent[Props] { props =>
      <.div(
        props.modifiers.toTagMod,
        ^.cls := "field",
        <.label(props.label, ^.htmlFor := props.id),
        <.div(
          <.data(props.value, ExploreStyles.StaticData, ^.id := props.id, ^.tabIndex := 0)
        )
      )
    }
}
