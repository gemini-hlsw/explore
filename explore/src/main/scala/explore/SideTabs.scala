// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import scala.scalajs.js.JSConverters._

import cats.implicits._
import explore.model.Page
import explore.model.SideButton
import explore.model.reusability._
import explore.components.ui.GPPStyles
import gpp.util.Zipper
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.common.implicits._
import react.semanticui.SemanticWidth
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.ButtonGroup
import react.semanticui.elements.divider.Divider
import react.semanticui.sizes._
import react.semanticui.widths._

final case class SideTabs[P](
  router: RouterCtl[P],
  tabs:   Zipper[SideButton]
) extends ReactProps[SideTabs[_]](SideTabs.component)

object SideTabs {
  type Props = SideTabs[_]

  implicit val reuse: Reusability[Props] = Reusability.by(_.tabs)

  private val component =
    ScalaComponent
      .builder[Props]
      .stateless
      .render_P { p =>
        val tabsL = p.tabs.toNel
        val focus = p.tabs.focus
        <.div(
          GPPStyles.SideTabsBody,
          VerticalSection()(Button(active = tabsL.head === focus)(tabsL.head.title)),
          Divider(hidden = true),
          VerticalSection()(
            ButtonGroup(widths = widthOf(tabsL.length))(
              // Due to the css rotations these need to be in reversed order
              tabsL.tail.reverse.map(b => Button(active = b === focus)(b.title)).toTagMod
            )
          )
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
