// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package react.reflex

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

import scalajs.js

trait ReflexEvents extends js.Object

object ReflexWithHandle {
  // Yes, this is a hack. We use it because react-reflex expects a ReactHandle to be the
  // direct first child of a ReactElement. This makes it impossible to make the ReactHandle
  // part of, say, a react-beautiful-dnd Droppable, which would have to go between the
  // ReactElement and the ReactHandle. Since ReactElement injects properties into its
  // ReactHandle child, things break down if there's anything in between.
  //
  // We are therefore using:
  // 1) An undocumented feature of react-reflex. If a boolean property "withHandle" is passed
  // to ReactElement, it will inject into its children the properties that ReactHandle needs.
  // (These properties are modeled in ReflexHandle.HandleProps).
  // 2) We create a Scala component that will take ReflexHandle.HandleProps + a render function,
  // to which it will provide the ReflexHandle.HandleProps. The render function can then pass this
  // to a ReactHandle within.
  // 3) Since the Scala component needs to capture the unboxed properties that ReactElement will
  // inject, we turn it into a JS component to avoid boxing, and we build a facade for it.
  // From our code, therefore, it is used as a JS component.
  //
  // For future reference, here is a permalink to where react-reflex injects properties for handle:
  // - https://github.com/leefsmp/Re-Flex/blob/850e1153ae3bddd3b0b1143b9fdb72a185027fd9/src/lib/ReflexElement.js#L198

  @js.native
  trait JsProps extends HandleProps with Props

  private val component = ScalaComponent
    .builder[JsProps]
    .render_P(p => p.render(p))
    .build

  private val jsComponent = component.toJsComponent.raw

  @js.native
  trait Props extends js.Object {
    var render: HandleProps => VdomNode
  }

  private val jsComponentFacade = JsFnComponent[Props, Children.None](jsComponent)

  private def props(render: HandleProps => VdomNode): Props = {
    val p = (new js.Object).asInstanceOf[Props]
    p.render = render
    p
  }

  def apply(render: HandleProps => VdomNode) = jsComponentFacade(props(render))

}
