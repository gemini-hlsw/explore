// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import cats.syntax.all._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.ReactCats._
import japgolly.scalajs.react.Reusability
import monocle.Focus
import monocle.Lens
import monocle.Traversal
import monocle.function.At._
import monocle.function.Each._
import react.gridlayout._

import scala.collection.immutable.SortedMap

/**
 * Utilities related to react-grid-layout
 */
object layout {
  type LayoutWidth = Int
  type LayoutCols  = Int
  type LayoutEntry = (LayoutWidth, LayoutCols, Layout)
  type LayoutsMap  = SortedMap[BreakpointName, (LayoutWidth, LayoutCols, Layout)]

  val LargeCutoff     = 1200
  val MediumCutoff    = 996
  val SmallCutoff     = Constants.TwoPanelCutoff.toInt
  val XtraSmallCutoff = 300

  val breakpoints: Map[BreakpointName, (LayoutWidth, LayoutCols)] =
    Map(
      (BreakpointName.lg, (LargeCutoff, 12)),
      (BreakpointName.md, (MediumCutoff, 10)),
      (BreakpointName.sm, (SmallCutoff, 8))
    )

  val layoutPprint =
    pprint.copy(
      additionalHandlers = { case layout: LayoutItem =>
        pprint.Tree.Literal(
          s"LayoutItem(id = ${layout.i}, x = ${layout.x}, y = ${layout.y}, w = ${layout.w}, h = ${layout.h}"
        )
      }
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

  implicit val nes: Ordering[NonEmptyString]                    = Order[NonEmptyString].toOrdering
  implicit val breakpointNameOrder: Order[BreakpointName]       = Order.by(_.name)
  implicit val breakpointNameOrdering: Ordering[BreakpointName] = breakpointNameOrder.toOrdering

  val allLayouts: Traversal[LayoutsMap, Layout] =
    each[LayoutsMap, LayoutEntry].andThen(Focus[LayoutEntry](_._3))

  def breakpointLayout(bn: BreakpointName): Traversal[LayoutsMap, Layout] =
    at[LayoutsMap, BreakpointName, Option[LayoutEntry]](bn).some.andThen(Focus[LayoutEntry](_._3))

  val layoutItem: Lens[Layout, List[LayoutItem]] = Focus[Layout](_.l)
  val layoutItems: Traversal[Layout, LayoutItem] = layoutItem.each

  val layoutItemName      = Focus[LayoutItem](_.i)
  val layoutItemHeight    = Focus[LayoutItem](_.h)
  val layoutItemMaxHeight = Focus[LayoutItem](_.maxH)
  val layoutItemMinHeight = Focus[LayoutItem](_.minH)
  val layoutItemWidth     = Focus[LayoutItem](_.w)
  val layoutItemX         = Focus[LayoutItem](_.x)
  val layoutItemY         = Focus[LayoutItem](_.y)
  val layoutItemResizable = Focus[LayoutItem](_.isResizable)

  val layoutsItemHeight = layoutItems.andThen(layoutItemHeight)

  implicit val breakpointNameReuse: Reusability[BreakpointName] = Reusability.byEq
  implicit val layoutItemReuse: Reusability[LayoutItem]         = Reusability.byEq
  implicit val layoutReuse: Reusability[Layout]                 = Reusability.byEq
  implicit val layoutsMapReuse: Reusability[LayoutsMap]         = Reusability.byEq

  object unsafe {
    implicit val layoutSemigroup: Semigroup[Layout] = Semigroup.instance { case (a, b) =>
      val result = a.l.foldLeft(List.empty[LayoutItem]) { case (l, la) =>
        b.l
          .find(_.i.exists(_ === la.i.get))
          .map { lb =>
            la |+| lb
          }
          .getOrElse(la) :: l
      }
      Layout(result)
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
    rhs.foldLeft(lhs) { case (acc, (k, v)) =>
      acc.get(k).fold(acc)(_ => acc.updated(k, optionCombine(v, acc.get(k))))
    }

}
