// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.primereact

import cats.syntax.all._
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.syntax.css.*
import react.common.*
import react.primereact.Slider

import scalajs.js
import scalajs.js.JSConverters.*

case class SliderView(
  id:       NonEmptyString,
  value:    View[Double],
  label:    String,
  disabled: js.UndefOr[Boolean] = js.undefined,
  clazz:    js.UndefOr[Css] = js.undefined
) extends ReactFnProps(SliderView.component)

object SliderView {
  private val component = ScalaFnComponent[SliderView] { props =>
    <.div(
      props.clazz.getOrElse(Css.Empty),
      <.label(^.htmlFor := props.id.value, props.label),
      Slider(id = props.id.value,
             value = props.value.get,
             onChange = props.value.set,
             disabled = props.disabled,
             clazz = props.clazz
      )
    )
  }
}
