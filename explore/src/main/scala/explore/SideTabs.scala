package explore

import cats.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.common.implicits._
import react.semanticui.elements.button.Button
import react.semanticui.sizes._
import react.semanticui.widths._
import explore.components.ui.GPPStyles
import react.semanticui.elements.button.ButtonGroup
import explore.Page
import react.semanticui.elements.divider.Divider

final case class SideTabs(c: RouterCtl[Page]) extends ReactProps[SideTabs](SideTabs.component)
// final case class SideTabs(c: RouterCtl[Page]) extends ReactProps[SideTabs](SideTabs.component)

object SideTabs {
  type Props = SideTabs

  private val component =
    ScalaComponent
      .builder[Props]
      .stateless
      .render_P { p =>
        println(p)
        <.div(
          GPPStyles.SideTabsBody,
          // Button(clazz = GPPStyles.VerticalButton2)("Overview2"),
          // Button(clazz = GPPStyles.VerticalButton2)("Overview3"),
          // Button("Overview"),
          // Button("Overview3"),
          <.div(
            GPPStyles.RotationWrapperOuter,
            <.div(
              GPPStyles.RotationWrapperInner,
              <.div(GPPStyles.VerticalButton, Button("Overview"))
            )
          ),
          Divider(hidden = true),
          // <.div(
          //   GPPStyles.RotationWrapperOuter,
          //   <.div(
          //     GPPStyles.RotationWrapperInner,
          //     <.div(GPPStyles.VerticalButton, Button("Overview"))
          //   )
          // ),
          // Button(size = Mini, clazz = GPPStyles.VerticalButton)("Test")
          <.div(
            GPPStyles.RotationWrapperOuter,
            <.div(
              GPPStyles.RotationWrapperInner,
              <.div(
                GPPStyles.VerticalButton,
                ButtonGroup(
                  // fluid = false,
                  widths = Four,
                  // clazz = GPPStyles.VerticalButton |+| GPPStyles.VerticalButtonGroup,
                  clazz = GPPStyles.VerticalButtonGroup
                  // size = Mini
                )(
                  // Due to the css rotations these need to be in reversed order
                  Button("Configurations"),
                  Button("Constraints"),
                  Button("Target"),
                  Button("Observations")
                )
              )
            )
          )
        )

      }
      .build
}
