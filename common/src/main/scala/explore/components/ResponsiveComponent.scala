// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.syntax.all._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.Css
import react.common.ReactFnPropsWithChildren
import react.resizeDetector.hooks._

case class ResponsiveComponent(
  widthBreakpoints:  List[(Int, Css)],
  heightBreakpoints: List[(Int, Css)] = Nil,
  clazz:             Css = Css.Empty
) extends ReactFnPropsWithChildren[ResponsiveComponent](ResponsiveComponent.component)

object ResponsiveComponent {
  type Props = ResponsiveComponent

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .withPropsChildren
      .useResizeDetector()
      .render { (p, c, s) =>
        val heightClass =
          p.heightBreakpoints.findLast(_._1 < s.height.orEmpty).foldMap(_._2)
        val widthClass  =
          p.widthBreakpoints.findLast(_._1 < s.width.orEmpty).foldMap(_._2)
        <.div(p.clazz |+| widthClass |+| heightClass, c).withRef(s.ref)
      }

}
