// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.syntax.all._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.common.implicits._
import react.resizeDetector.ResizeDetector

final case class ResponsiveComponent(
  widthBreakpoints:  List[(Int, Css)],
  heightBreakpoints: List[(Int, Css)] = Nil,
  clazz:             Css = Css.Empty
) extends ReactPropsWithChildren[ResponsiveComponent](ResponsiveComponent.component)

object ResponsiveComponent {
  type Props = ResponsiveComponent

  // Explicitly never reuse as we are not considering the content
  implicit val propsReuse: Reusability[ResponsiveComponent] = Reusability.never

  val component =
    ScalaComponent
      .builder[Props]
      .stateless
      .render_PC { (p, c) =>
        ResizeDetector() { s =>
          val heightClass =
            p.heightBreakpoints.findLast(_._1 < s.height.orEmpty).foldMap(_._2)
          val widthClass  =
            p.widthBreakpoints.findLast(_._1 < s.width.orEmpty).foldMap(_._2)
          <.div(p.clazz |+| widthClass |+| heightClass, s.targetRef, c)
        }
      }
      .configure(Reusability.shouldComponentUpdate)
      .build

}
