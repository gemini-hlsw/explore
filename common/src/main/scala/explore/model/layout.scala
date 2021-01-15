// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import eu.timepit.refined.types.numeric.NonNegInt
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.raw.JsNumber
import lucuma.ui.reusability._
import monocle.function.Field3._
import monocle.function.Index._
import monocle.macros.GenLens
import monocle.{ Lens, Optional }
import react.gridlayout._

import scala.collection.immutable.SortedMap

/**
 * Utilities related to react-grid-layout
 */
object layout {
  type LayoutEntry = (JsNumber, JsNumber, Layout)
  type LayoutsMap  = SortedMap[BreakpointName, (JsNumber, JsNumber, Layout)]

  val layoutItem       = GenLens[Layout](_.l)
  val layoutItemHeight = GenLens[LayoutItem](_.h)
  val layoutItemWidth  = GenLens[LayoutItem](_.w)
  val layoutItemX      = GenLens[LayoutItem](_.x)
  val layoutItemY      = GenLens[LayoutItem](_.y)

  implicit class LayoutsOps[A](val layouts: Lens[A, LayoutsMap]) extends AnyVal {
    def breakPoint(n:       BreakpointName): Optional[A, LayoutEntry] = layouts.composeOptional(index(n))
    def breakPointLayout(n: BreakpointName, pos: NonNegInt): Optional[A, LayoutItem] =
      breakPoint(n).composeLens(third).composeLens(layoutItem).composeOptional(index(pos.value))
  }

  implicit val breakpointNameReuse: Reusability[BreakpointName] = Reusability.by(_.name)
  implicit val layoutItemReuse: Reusability[LayoutItem]         = Reusability.derive
  implicit val layoutReuse: Reusability[Layout]                 = Reusability.by(_.l)
  implicit val layoutsMapReuse: Reusability[LayoutsMap]         = Reusability.by(_.toList)
}
