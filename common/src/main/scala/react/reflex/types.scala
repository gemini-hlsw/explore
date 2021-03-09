// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package react.reflex

import japgolly.scalajs.react._
import org.scalajs.dom
import react.common.EnumValue

import scalajs.js
import scalajs.js.|

@js.native
trait ResizeEvent extends js.Object {
  val domElement: dom.html.Element
  val component: JsComponent.RawMounted[ReflexElement.Props, js.Object]
}

sealed trait Orientation extends Product with Serializable

object Orientation {
  implicit val enum: EnumValue[Orientation] = EnumValue.toLowerCaseString

  case object Horizontal extends Orientation
  case object Vertical   extends Orientation
}

sealed abstract class Direction(val toJs: Direction.JsType) extends Product with Serializable

object Direction {
  type JsType = Int | js.Array[Int]

  case object Forward   extends Direction(1)
  case object Backwards extends Direction(-1)
  case object Both      extends Direction(js.Array(-1, 1))
}
