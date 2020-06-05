// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import scala.scalajs.js.JSConverters._

import cats.implicits._
import explore.Page
import explore.components.ui.GPPStyles
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

final case class SideButton(title: String)

object SideButton {
  implicit val reuse: Reusability[SideButton] =
    Reusability.derive
}

final case class SideTabs(
  router:         RouterCtl[Page],
  topButton:      Option[SideButton],
  sectionButtons: List[SideButton]
) extends ReactProps[SideTabs](SideTabs.component)

object SideTabs {
  type Props = SideTabs

  // TODO Move this to semanticui-react
  val allWidths: Map[Int, SemanticWidth] =
    Map(
      1  -> One,
      2  -> Two,
      3  -> Three,
      4  -> Four,
      5  -> Five,
      6  -> Six,
      7  -> Seven,
      8  -> Eight,
      9  -> Nine,
      10 -> Ten,
      11 -> Eleven,
      12 -> Twelve,
      13 -> Thirteen,
      14 -> Fourteen,
      15 -> Fifteen,
      16 -> Sixteen
    )

  implicit val reuse: Reusability[Props] = Reusability.caseClassExcept("router")

  private val component =
    ScalaComponent
      .builder[Props]
      .stateless
      .render_P(p =>
        <.div(
          GPPStyles.SideTabsBody,
          p.topButton.map(b => VerticalSection()(Button(b.title))),
          Divider(hidden = true),
          VerticalSection()(
            ButtonGroup(widths = allWidths.get(p.sectionButtons.length).orUndefined)(
              // Due to the css rotations these need to be in reversed order
              p.sectionButtons.reverse.map(b => Button(b.title)).toTagMod
            )
          )
        )
      )
      .configure(Reusability.shouldComponentUpdate)
      .build
}

/**
  * Component that uses css tricks to support properly rotated components
  * respecting the layout. see:
  * https://stackoverflow.com/questions/16301625/rotated-elements-in-css-that-affect-their-parents-height-correctly
  * It requires css to work properly
  */
final case class VerticalSection()
    extends ReactPropsWithChildren[VerticalSection](VerticalSection.component)

object VerticalSection {
  type Props = VerticalSection

  implicit val reuse: Reusability[Props] = Reusability.always

  private val component =
    ScalaComponent
      .builder[Props]
      .stateless
      .render_C(c =>
        <.div(
          GPPStyles.RotationWrapperOuter,
          <.div(
            GPPStyles.RotationWrapperInner,
            <.div(GPPStyles.VerticalButton, c)
          )
        )
      )
      .configure(Reusability.shouldComponentUpdate)
      .build
}
