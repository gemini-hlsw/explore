// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Order
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegInt
import explore.model.enums.GridLayoutSection
import explore.model.layout.LayoutsMap
import explore.model.layout.*
import lucuma.refined.*
import react.gridlayout.BreakpointName
import react.gridlayout.Layout
import react.gridlayout.LayoutItem

import scala.collection.immutable.SortedMap

/**
 * Default grid layout defiinitions
 */
object ExploreGridLayouts:
  private given Order[BreakpointName] = Order.by(_.name)

  def sectionLayout: GridLayoutSection => LayoutsMap = _ match {
    case GridLayoutSection.ProgramsLayout     => programs.defaultProgramsLayouts
    case GridLayoutSection.ConstraintsLayout  => constraints.defaultConstraintsLayouts
    case GridLayoutSection.SchedulingLayout   => scheduling.defaultSchedulingLayouts
    case GridLayoutSection.TargetLayout       => targets.defaultTargetLayouts
    case GridLayoutSection.ObservationsLayout => observations.defaultObsLayouts
    case _                                    => SortedMap.empty
  }

  val DefaultLayouts: Map[GridLayoutSection, LayoutsMap] =
    SortedMap(GridLayoutSection.values.map(l => l -> sectionLayout(l)): _*)

  object constraints:
    private val ConstraintsHeight: NonNegInt   = 4.refined
    private val TimingWindowsHeight: NonNegInt = 14.refined
    private val DefaultWidth: NonNegInt        = 10.refined
    private val DefaultLargeWidth: NonNegInt   = 12.refined

    private val layoutMedium: Layout = Layout(
      List(
        LayoutItem(
          i = ObsTabTilesIds.ConstraintsId.id.value,
          x = 0,
          y = 0,
          w = DefaultWidth.value,
          h = ConstraintsHeight.value,
          isResizable = false
        ),
        LayoutItem(
          i = ObsTabTilesIds.TimingWindowsId.id.value,
          x = 0,
          y = ConstraintsHeight.value,
          w = DefaultWidth.value,
          h = TimingWindowsHeight.value,
          isResizable = false
        )
      )
    )

    val defaultConstraintsLayouts = defineStdLayouts(
      Map(
        (BreakpointName.lg,
         layoutItems.andThen(layoutItemWidth).replace(DefaultLargeWidth)(layoutMedium)
        ),
        (BreakpointName.md, layoutMedium)
      )
    )

  object scheduling:
    private val SchedulingHeight: NonNegInt  = 14.refined
    private val DefaultWidth: NonNegInt      = 10.refined
    private val DefaultLargeWidth: NonNegInt = 12.refined

    private val layoutMedium: Layout = Layout(
      List(
        LayoutItem(
          i = ObsTabTilesIds.TimingWindowsId.id.value,
          x = 0,
          y = 0,
          w = DefaultWidth.value,
          h = SchedulingHeight.value,
          isResizable = false
        )
      )
    )

    val defaultSchedulingLayouts = defineStdLayouts(
      Map(
        (BreakpointName.lg,
         layoutItems.andThen(layoutItemWidth).replace(DefaultLargeWidth)(layoutMedium)
        ),
        (BreakpointName.md, layoutMedium)
      )
    )

  object targets:
    private val SummaryHeight: NonNegInt     = 6.refined
    private val SummaryMinHeight: NonNegInt  = 4.refined
    private val TargetHeight: NonNegInt      = 18.refined
    private val TargetMinHeight: NonNegInt   = 15.refined
    private val SkyPlotHeight: NonNegInt     = 9.refined
    private val SkyPlotMinHeight: NonNegInt  = 6.refined
    private val TileMinWidth: NonNegInt      = 5.refined
    private val DefaultWidth: NonNegInt      = 10.refined
    private val DefaultLargeWidth: NonNegInt = 12.refined

    private val layoutMedium: Layout = Layout(
      List(
        LayoutItem(
          i = ObsTabTilesIds.TargetSummaryId.id.value,
          x = 0,
          y = 0,
          w = DefaultWidth.value,
          h = SummaryHeight.value,
          minH = SummaryMinHeight.value,
          minW = TileMinWidth.value
        ),
        LayoutItem(
          i = ObsTabTilesIds.TargetId.id.value,
          x = 0,
          y = SummaryHeight.value,
          w = DefaultWidth.value,
          h = TargetHeight.value,
          minH = TargetMinHeight.value,
          minW = TileMinWidth.value
        ),
        LayoutItem(
          i = ObsTabTilesIds.PlotId.id.value,
          x = 0,
          y = SummaryHeight.value + TargetHeight.value,
          w = DefaultWidth.value,
          h = SkyPlotHeight.value,
          minH = SkyPlotMinHeight.value,
          minW = TileMinWidth.value
        )
      )
    )

    val defaultTargetLayouts = defineStdLayouts(
      Map(
        (BreakpointName.lg,
         layoutItems.andThen(layoutItemWidth).replace(DefaultLargeWidth)(layoutMedium)
        ),
        (BreakpointName.md, layoutMedium)
      )
    )

    private val singleLayoutMedium: Layout = Layout(
      List(
        LayoutItem(
          i = ObsTabTilesIds.TargetSummaryId.id.value,
          x = 0,
          y = 0,
          w = DefaultWidth.value,
          h = 0, // This doesn't matter, we are forcing 100%.
          minH = SummaryMinHeight.value,
          minW = TileMinWidth.value,
          static = true
        )
      )
    )

    val defaultSingleLayouts = defineStdLayouts(
      Map(
        (BreakpointName.lg,
         layoutItems.andThen(layoutItemWidth).replace(DefaultLargeWidth)(singleLayoutMedium)
        ),
        (BreakpointName.md, singleLayoutMedium)
      )
    )

  object observations:
    private val NotesMaxHeight: NonNegInt         = 3.refined
    private val TargetHeight: NonNegInt           = 18.refined
    private val TargetMinHeight: NonNegInt        = 15.refined
    private val SkyPlotHeight: NonNegInt          = 9.refined
    private val SkyPlotMinHeight: NonNegInt       = 6.refined
    private val ConstraintsMinHeight: NonNegInt   = 4.refined
    private val ConstraintsMaxHeight: NonNegInt   = 7.refined
    private val TimingWindowsMinHeight: NonNegInt = 8.refined
    private val TimingWindowsMaxHeight: NonNegInt = 12.refined
    private val ConfigurationMaxHeight: NonNegInt = 10.refined
    private val ItcMaxHeight: NonNegInt           = 9.refined
    private val FinderChartMinHeight: NonNegInt   = 6.refined
    private val FinderChartHeight: NonNegInt      = 9.refined
    private val DefaultWidth: NonNegInt           = 10.refined
    private val TileMinWidth: NonNegInt           = 6.refined
    private val DefaultLargeWidth: NonNegInt      = 12.refined

    private val layoutMedium: Layout = Layout(
      List(
        LayoutItem(
          x = 0,
          y = 0,
          w = DefaultWidth.value,
          h = NotesMaxHeight.value,
          i = ObsTabTilesIds.NotesId.id.value,
          isResizable = false
        ),
        LayoutItem(
          x = 0,
          y = NotesMaxHeight.value,
          w = DefaultWidth.value,
          h = TargetHeight.value,
          minH = TargetMinHeight.value,
          minW = TileMinWidth.value,
          i = ObsTabTilesIds.TargetId.id.value
        ),
        LayoutItem(
          x = 0,
          y = (NotesMaxHeight |+| TargetHeight).value,
          w = DefaultWidth.value,
          h = FinderChartHeight.value,
          minH = FinderChartMinHeight.value,
          minW = TileMinWidth.value,
          i = ObsTabTilesIds.FinderChartsId.id.value
        ),
        LayoutItem(
          x = 0,
          y = (NotesMaxHeight |+| TargetHeight |+| FinderChartHeight).value,
          w = DefaultWidth.value,
          h = SkyPlotHeight.value,
          minH = SkyPlotMinHeight.value,
          minW = TileMinWidth.value,
          i = ObsTabTilesIds.PlotId.id.value
        ),
        LayoutItem(
          x = 0,
          y = (NotesMaxHeight |+| TargetHeight |+| FinderChartHeight |+| SkyPlotHeight).value,
          w = DefaultWidth.value,
          h = ConstraintsMaxHeight.value,
          minH = ConstraintsMinHeight.value,
          maxH = ConstraintsMaxHeight.value,
          minW = TileMinWidth.value,
          i = ObsTabTilesIds.ConstraintsId.id.value
        ),
        LayoutItem(
          x = 0,
          y =
            (NotesMaxHeight |+| TargetHeight |+| FinderChartHeight |+| SkyPlotHeight |+| ConstraintsMaxHeight).value,
          w = DefaultWidth.value,
          h = TimingWindowsMaxHeight.value,
          minH = TimingWindowsMinHeight.value,
          maxH = TimingWindowsMaxHeight.value,
          minW = TileMinWidth.value,
          i = ObsTabTilesIds.TimingWindowsId.id.value
        ),
        LayoutItem(
          x = 0,
          y =
            (NotesMaxHeight |+| TargetHeight |+| FinderChartHeight |+| SkyPlotHeight |+| ConstraintsMaxHeight |+| TimingWindowsMaxHeight).value,
          w = DefaultWidth.value,
          h = ConfigurationMaxHeight.value,
          i = ObsTabTilesIds.ConfigurationId.id.value
        ),
        LayoutItem(
          x = 0,
          y =
            (NotesMaxHeight |+| TargetHeight |+| FinderChartHeight |+| SkyPlotHeight |+| ConstraintsMaxHeight |+| TimingWindowsMaxHeight |+| ConfigurationMaxHeight).value,
          w = DefaultWidth.value,
          h = ItcMaxHeight.value,
          i = ObsTabTilesIds.ItcId.id.value
        )
      )
    )

    val defaultObsLayouts: LayoutsMap =
      defineStdLayouts(
        Map(
          (BreakpointName.lg,
           layoutItems.andThen(layoutItemWidth).replace(DefaultLargeWidth)(layoutMedium)
          ),
          (BreakpointName.md, layoutMedium)
        )
      )

  object programs:
    private val DetailsHeight: NonNegInt           = 6.refined
    private val DetailsMinHeight: NonNegInt        = 4.refined
    private val NotesHeight: NonNegInt             = 6.refined
    private val NotesMinHeight: NonNegInt          = 4.refined
    private val ChangeRequestsHeight: NonNegInt    = 6.refined
    private val ChangeRequestsMinHeight: NonNegInt = 4.refined
    private val TileMinWidth: NonNegInt            = 8.refined
    private val DefaultWidth: NonNegInt            = 10.refined
    private val DefaultLargeWidth: NonNegInt       = 12.refined

    private val layoutMedium: Layout = Layout(
      List(
        LayoutItem(
          i = ProgramTabTileIds.DetailsId.id.value,
          x = 0,
          y = 0,
          w = DefaultWidth.value,
          h = DetailsHeight.value,
          minH = DetailsMinHeight.value,
          minW = TileMinWidth.value
        ),
        LayoutItem(
          i = ProgramTabTileIds.NotesId.id.value,
          x = 0,
          y = DetailsHeight.value,
          w = DefaultWidth.value,
          h = NotesHeight.value,
          minH = NotesMinHeight.value,
          minW = TileMinWidth.value
        ),
        LayoutItem(
          i = ProgramTabTileIds.ChangeRequestsId.id.value,
          x = 0,
          y = (DetailsHeight |+| NotesHeight).value,
          w = DefaultWidth.value,
          h = ChangeRequestsHeight.value,
          minH = ChangeRequestsMinHeight.value,
          minW = TileMinWidth.value
        )
      )
    )

    val defaultProgramsLayouts = defineStdLayouts(
      Map(
        (BreakpointName.lg,
         layoutItems.andThen(layoutItemWidth).replace(DefaultLargeWidth)(layoutMedium)
        ),
        (BreakpointName.md, layoutMedium)
      )
    )
