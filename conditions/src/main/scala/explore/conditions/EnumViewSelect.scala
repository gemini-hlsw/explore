// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.conditions

import scala.scalajs.js.JSConverters._

import cats.Show
import cats.implicits._
import gem.util.Enumerated
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common.ReactProps
import react.semanticui.addons.select.Select
import react.semanticui.modules.dropdown._
import crystal.ViewOptF
import crystal.react.implicits._
import cats.effect.Effect
import scalajs.js

/**
  * Produces a dropdown menu, similar to a combobox
  */
final case class EnumViewSelect[F[_], A](
  label:       String,
  value:       ViewOptF[F, A],
  placeholder: js.UndefOr[String] = js.undefined,
  disabled:    js.UndefOr[Boolean] = js.undefined
)(implicit
  val enum:    Enumerated[A],
  val show:    Show[A],
  val effect:  Effect[F]
) extends ReactProps[EnumViewSelect[Any, Any]](EnumViewSelect.component)

object EnumViewSelect {
  type Props[F[_], A] = EnumViewSelect[F, A]

  implicit protected def propsReuse[F[_], A]: Reusability[Props[F, A]] =
    Reusability.by(p => (p.label, p.value.get.map(p.show.show), p.placeholder, p.disabled))

  protected val component =
    ScalaComponent
      .builder[Props[Any, Any]]
      .stateless
      .render_P { p =>
        implicit val show   = p.show
        implicit val effect = p.effect

        <.div(
          ^.cls := "field",
          <.label(p.label),
          Select(
            placeholder = p.placeholder,
            fluid = true,
            disabled = p.disabled,
            value = p.value.get.map(i => p.enum.tag(i)).orUndefined,
            // value = p.enum.tag(p.value.get),
            options = p.enum.all
              .map(i => DropdownItem(text = i.show, value = p.enum.tag(i))),
            onChange = (ddp: Dropdown.DropdownProps) =>
              ddp.value.toOption
                .flatMap(v => p.enum.fromTag(v.asInstanceOf[String]))
                .map(v => p.value.set(v).runInCB)
                .getOrEmpty
          )
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
