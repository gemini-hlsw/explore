// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import explore.components.ui.ExploreStyles
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.TagMod
import japgolly.scalajs.react.vdom.html_<^._
import react.common._

/**
 * A control similar to a readonly FormInput styled as `static-data`, except that this control will
 * be only as wide as it needs to be.
 */

final case class FormStaticData(
  id:        String,
  value:     TagMod,
  label:     String,
  modifiers: Seq[TagMod] = Seq.empty
) extends ReactProps[FormStaticData](FormStaticData.component) {
  def apply(mods: TagMod*): FormStaticData = copy(modifiers = modifiers ++ mods)
}

object FormStaticData {
  type Props = FormStaticData

  val component =
    ScalaComponent
      .builder[Props]
      .render_P { props =>
        <.div(
          props.modifiers.toTagMod,
          ^.cls := "field",
          <.label(props.label, ^.htmlFor := props.id),
          <.div(^.cls                    := "ui input",
                <.data(props.value, ExploreStyles.StaticData, ^.id := props.id, ^.tabIndex := 0)
          )
        )
      }
      .build
}
