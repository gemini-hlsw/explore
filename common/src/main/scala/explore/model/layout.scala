// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import cats.syntax.all._
import eu.timepit.refined.types.numeric.NonNegInt
import japgolly.scalajs.react.Reusability
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
  type LayoutEntry = (Int, Int, Layout)
  type LayoutsMap  = SortedMap[BreakpointName, (Int, Int, Layout)]

  val breakpoints: Map[BreakpointName, (Int, Int)] =
    Map(
      (BreakpointName.lg, (1200, 12)),
      (BreakpointName.md, (996, 10)),
      (BreakpointName.sm, (768, 8))
    )

  def defineStdLayouts(l: Map[BreakpointName, Layout]): LayoutsMap =
    l.map { case (n, l) =>
      (n, (breakpointWidth(n): Int, breakpointCols(n): Int, l))
    }.to(SortedMap)

  def breakpointNameFromString(s: String): BreakpointName =
    BreakpointName.predefined.find(_.name === s).getOrElse(sys.error("Undefined breakpoint"))

  def breakpointWidth(b: BreakpointName): Int =
    breakpoints.get(b).foldMap(_._1)

  def breakpointCols(b: BreakpointName): Int =
    breakpoints.get(b).foldMap(_._2)

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
  implicit val breakpointNameOrder: Order[BreakpointName]       = Order.by(_.name)
  implicit val breakpointNameOrdering: Ordering[BreakpointName] = breakpointNameOrder.toOrdering

  object unsafe {
    implicit val layoutSemigroup: Semigroup[Layout] = Semigroup.instance { case (a, b) =>
      Layout(a.l |+| b.l)
    }

    implicit val layoutItemSemigroup: Semigroup[LayoutItem] = Semigroup.instance { case (a, b) =>
      a.copy(w = b.w, h = b.h, x = b.x, y = b.y)
    }

    implicit val layoutGroupSemigroup: Semigroup[(Int, Int, Layout)] =
      Semigroup.instance { case ((a, b, c), (_, _, d)) =>
        (a, b, c |+| d)
      }
  }

  def optionCombine[A: Semigroup](a: A, opt: Option[A]): A =
    opt.map(a |+| _).getOrElse(a)

  def mergeMap[K, V: Semigroup](lhs: SortedMap[K, V], rhs: SortedMap[K, V]): SortedMap[K, V] =
    lhs.foldLeft(rhs) { case (acc, (k, v)) =>
      acc.updated(k, optionCombine(v, acc.get(k)))
    }

}
