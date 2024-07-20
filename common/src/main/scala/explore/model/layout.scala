// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.*
import cats.Order.*
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import explore.model.enums.GridLayoutSection
import lucuma.react.gridlayout.*
import lucuma.refined.*
import monocle.Focus
import monocle.Iso
import monocle.Lens
import monocle.Traversal
import monocle.function.At.*
import monocle.function.Each
import monocle.function.Each.*

import scala.collection.immutable.SortedMap

/**
 * Utilities related to react-grid-layout
 */
object layout {
  type LayoutWidth       = Int
  type LayoutCols        = Int
  type LayoutEntry       = (LayoutWidth, LayoutCols, Layout)
  type LayoutsMap        = SortedMap[BreakpointName, LayoutEntry]
  type SectionLayoutsMap = Map[GridLayoutSection, LayoutsMap]

  val LargeCutoff     = 1200
  val MediumCutoff    = 996
  val SmallCutoff     = Constants.TwoPanelCutoff.toInt
  val XtraSmallCutoff = 300

  lazy val DefaultWidth: NonNegInt      = 16.refined
  lazy val DefaultLargeWidth: NonNegInt = 32.refined

  // Restricted to GridRowHeight * 10 = 360px which is basically the min width for a mobile device
  lazy val TileMinWidth: NonNegInt = 8.refined

  val breakpoints: Map[BreakpointName, (LayoutWidth, LayoutCols)] =
    Map(
      (BreakpointName.lg, (LargeCutoff, DefaultLargeWidth.value)),
      (BreakpointName.md, (MediumCutoff, DefaultWidth.value)),
      (BreakpointName.sm, (SmallCutoff, TileMinWidth.value))
    )

  val layoutPprint =
    pprint.copy(
      additionalHandlers = { case layout: LayoutItem =>
        pprint.Tree.Literal(
          s"LayoutItem(id = ${layout.i}, x = ${layout.x}, y = ${layout.y}, w = ${layout.w}, h = ${layout.h})"
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

  given Order[BreakpointName] = Order.by(_.name)

  val allLayouts: Traversal[LayoutsMap, Layout] =
    each[LayoutsMap, LayoutEntry].andThen(Focus[LayoutEntry](_._3))

  def breakpointLayout(bn: BreakpointName): Traversal[LayoutsMap, Layout] =
    at[LayoutsMap, BreakpointName, Option[LayoutEntry]](bn).some.andThen(Focus[LayoutEntry](_._3))

  given Each[Layout, LayoutItem] =
    Each.fromIso(Iso[Layout, List[LayoutItem]](_.asList)(Layout.apply))

  extension (l: LayoutsMap)
    def breakpointProportionalWidth(
      current: BreakpointName,
      next:    BreakpointName
    ): LayoutsMap = {
      val maxCols  = breakpointCols(current)
      val nextCols = breakpointCols(next)
      val ratio    = nextCols.toDouble / maxCols.toDouble

      breakpointLayout(next).each.modify { li =>
        if (ratio < 1) li
        else li.copy(w = (li.w * ratio).toInt)
      }(l)
    }

  val layoutItem: Lens[Layout, List[LayoutItem]] =
    Lens[Layout, List[LayoutItem]](_.asList)(list => _ => Layout(list))
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

  // Only the x, y, width and height are saved in user preferences.
  def mergeLayoutItems(current: LayoutItem, fromDb: LayoutItem): LayoutItem =
    current.copy(w = fromDb.w, h = fromDb.h, x = fromDb.x, y = fromDb.y)

  // As with the other merges, the current Layout is expected to have all
  // of required LayoutItems since it originates with the DefaultLayout.
  // So, any extra LayoutItems from the db are ignored.
  def mergeLayouts(current: Layout, fromDb: Layout): Layout =
    val list = current.asList.foldLeft(List.empty[LayoutItem]) { (acc, currentItem) =>
      fromDb.asList
        .find(_.i === currentItem.i)
        .map(dbItem => mergeLayoutItems(currentItem, dbItem))
        .getOrElse(currentItem) :: acc
    }
    Layout(list.reverse)

  def mergeLayoutEntries(current: LayoutEntry, fromDb: LayoutEntry): LayoutEntry =
    (current._1, current._2, mergeLayouts(current._3, fromDb._3))

  def mergeLayoutsMaps(current: LayoutsMap, fromDb: LayoutsMap) =
    fromDb.foldLeft(current) { case (acc, (breakpoint, dbEntry)) =>
      acc
        .get(breakpoint)
        .fold(acc)(currentEntry =>
          acc.updated(breakpoint, mergeLayoutEntries(currentEntry, dbEntry))
        )
    }

  // Updates the x, y, width, height of the layout items in `current` from the ones in `fromDb`.
  // The `current` map is expected to have all of the required sections, layouts, entries, and items
  // because it originated with the DefaultLayouts. Anything "extra" in `fromDb` will be ignored.
  def mergeSectionLayoutsMaps(
    current: SectionLayoutsMap,
    fromDb:  SectionLayoutsMap
  ): SectionLayoutsMap =
    fromDb.foldLeft(current) { case (acc, (section, dbLayouts)) =>
      acc
        .get(section)
        .fold(acc)(currentLayouts =>
          acc.updated(section, mergeLayoutsMaps(currentLayouts, dbLayouts))
        )

    }
}
