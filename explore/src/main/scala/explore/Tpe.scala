// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import explore.implicits._
import cats.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scala.scalajs.js
import js.JSConverters._
import js.UndefOr._
import model.Target
import react.common._
import explore.model._
import react.semanticui.elements.button.Button
import react.semanticui.colors._
import crystal.react.implicits._

final case class Tpe(target: ViewCtxIO[Option[Target]]) extends ReactProps {
  @inline def render: VdomElement = Tpe.component(this)
}

object Tpe {
  type Props = Tpe

  private implicit val propsReuse: Reusability[Props] = Reusability.derive

  trait ViewOpts extends js.Object {
    var fov: Double
    var target: String
  }

  private val component =
    ScalaComponent
      .builder[Props]("TPE")
      .render_P { props =>
        def renderButton(forTarget: Target, selected: Option[Target]) = {
          val color = selected.filter(_ == forTarget).map(_ => Blue).orUndefined
          Button(onClick = props.target.view.set(forTarget.some).toCB, color = color)(
            forTarget.toString
          )
        }

        <.div(
          ^.height := 28.pc,
          <.div(
            Button("IQ"),
            Button(color = Blue)("Button", "Btn"),
            Button("Button", "Dec")
          ),
          <.div(
            List(Target.M81, Target.M51).toTagMod(target =>
              renderButton(target, props.target.view.value)
            )
          )
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
