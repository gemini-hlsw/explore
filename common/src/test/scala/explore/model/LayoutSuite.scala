// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import explore.model.enums.GridLayoutSection
import explore.model.layout.*
import lucuma.react.gridlayout.*
import munit.FunSuite

import scala.collection.immutable.SortedMap

class LayoutSuite extends FunSuite {

  val observationMdXLayoutItem =
    LayoutItem(i = "X", x = 0, y = 0, w = 8, h = 5, minH = 3, maxH = 10, minW = 2, maxW = 12)

  val dbObservationMdXLayoutItem =
    LayoutItem(i = "X", x = 1, y = 2, w = 7, h = 9, minH = 0, maxH = 15, minW = 5, maxW = 11)

  val expectedObservationMdXLayoutItem =
    observationMdXLayoutItem.copy(x = 1, y = 2, w = 7, h = 9)

  val observationMdLayout = Layout(
    List[LayoutItem](
      observationMdXLayoutItem,
      LayoutItem(i = "Y", x = 5, y = 9, w = 7, h = 8, minH = 6, maxH = 9, minW = 4, maxW = 11),
      LayoutItem(i = "Z", x = 9, y = 15, w = 8, h = 9, minH = 5, maxH = 7, minW = 7, maxW = 9)
    )
  )

  val dbObservationMdLayout = Layout(
    List[LayoutItem](
      dbObservationMdXLayoutItem,
      LayoutItem(i = "Y", x = 3, y = 2, w = 8, h = 6, minH = 5, maxH = 11, minW = 3, maxW = 12),
      LayoutItem(i = "Q", x = 7, y = 14, w = 8, h = 9, minH = 5, maxH = 7, minW = 7, maxW = 9)
    )
  )

  val expectedObservationMdLayout = Layout(
    List[LayoutItem](
      expectedObservationMdXLayoutItem,
      LayoutItem(i = "Y", x = 3, y = 2, w = 8, h = 6, minH = 6, maxH = 9, minW = 4, maxW = 11),
      LayoutItem(i = "Z", x = 9, y = 15, w = 8, h = 9, minH = 5, maxH = 7, minW = 7, maxW = 9)
    )
  )

  val originalMap: SectionLayoutsMap = Map(
    GridLayoutSection.ObservationsLayout -> SortedMap(
      BreakpointName.md -> (8, 10, observationMdLayout),
      BreakpointName.lg -> (7,
                            11,
                            Layout(
                              List[LayoutItem](
                                LayoutItem(i = "X",
                                           x = 2,
                                           y = 1,
                                           w = 6,
                                           h = 6,
                                           minH = 5,
                                           maxH = 11,
                                           minW = 3,
                                           maxW = 10
                                )
                              )
                            )
      )
    ),
    GridLayoutSection.ConstraintsLayout  -> SortedMap(
      BreakpointName.md -> (9,
                            13,
                            Layout(
                              List[LayoutItem](
                                LayoutItem(i = "Z",
                                           x = 0,
                                           y = 0,
                                           w = 8,
                                           h = 5,
                                           minH = 3,
                                           maxH = 10,
                                           minW = 2,
                                           maxW = 12
                                )
                              )
                            )
      )
    )
  )

  val dbMap: SectionLayoutsMap = Map(
    GridLayoutSection.ObservationsLayout -> SortedMap(
      BreakpointName.md  -> (9, 20, dbObservationMdLayout),
      BreakpointName.xxs -> (7,
                             12,
                             Layout(
                               List[LayoutItem](
                                 LayoutItem(i = "X",
                                            x = 5,
                                            y = 1,
                                            w = 6,
                                            h = 6,
                                            minH = 5,
                                            maxH = 11,
                                            minW = 3,
                                            maxW = 10
                                 )
                               )
                             )
      )
    ),
    GridLayoutSection.SchedulingLayout   -> SortedMap(
      BreakpointName.md -> (
        8,
        10,
        Layout(
          List[LayoutItem](
            LayoutItem(i = "Y", x = 5, y = 9, w = 9, h = 7, minH = 4, maxH = 9, minW = 4, maxW = 11)
          )
        )
      )
    )
  )

  val expectedMap: SectionLayoutsMap = Map(
    GridLayoutSection.ObservationsLayout -> SortedMap(
      BreakpointName.md -> (8, 10, expectedObservationMdLayout),
      BreakpointName.lg -> (7,
                            11,
                            Layout(
                              List[LayoutItem](
                                LayoutItem(i = "X",
                                           x = 2,
                                           y = 1,
                                           w = 6,
                                           h = 6,
                                           minH = 5,
                                           maxH = 11,
                                           minW = 3,
                                           maxW = 10
                                )
                              )
                            )
      )
    ),
    GridLayoutSection.ConstraintsLayout  -> SortedMap(
      BreakpointName.md -> (9,
                            13,
                            Layout(
                              List[LayoutItem](
                                LayoutItem(i = "Z",
                                           x = 0,
                                           y = 0,
                                           w = 8,
                                           h = 5,
                                           minH = 3,
                                           maxH = 10,
                                           minW = 2,
                                           maxW = 12
                                )
                              )
                            )
      )
    )
  )

  test("LayoutItems merge correctly") {
    val mergedLayoutItem = mergeLayoutItems(observationMdXLayoutItem, dbObservationMdXLayoutItem)
    assertEquals(mergedLayoutItem, expectedObservationMdXLayoutItem)
  }

  test("Layouts merge correctly") {
    val mergedLayout = mergeLayouts(observationMdLayout, dbObservationMdLayout)
    assertEquals(mergedLayout, expectedObservationMdLayout)
  }

  test("SectionLayoutsMaps merge correctly") {
    val mergedMap = mergeSectionLayoutsMaps(originalMap, dbMap)
    assertEquals(mergedMap, expectedMap)
  }
}
