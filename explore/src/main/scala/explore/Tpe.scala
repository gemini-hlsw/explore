// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scala.scalajs.js
import model.Target
import react.common._
import explore.model._

final case class Tpe(target: Target) extends ReactProps {
  @inline def render: VdomElement = Tpe.component(this)
}

object Tpe {
  type Props = Tpe

  trait ViewOpts extends js.Object {
    var fov: Double
    var target: String
  }

  private val component =
    ScalaComponent
      .builder[Props]("TPE")
      .render { _ =>
        <.div(
          ^.height := 28.pc
        )(
          Views.persons.flow { persons =>
            <.div(persons.toString)
          }
        )
      }
      .build
}
