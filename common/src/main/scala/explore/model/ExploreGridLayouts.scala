// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegInt
import explore.model.enums.GridLayoutSection
import explore.model.layout.*
import explore.model.layout.LayoutsMap
import lucuma.react.gridlayout.BreakpointName
import lucuma.react.gridlayout.Layout
import lucuma.react.gridlayout.LayoutItem
import lucuma.refined.*

import scala.collection.immutable.SortedMap

/**
 * Default grid layout defiinitions
 */
object ExploreGridLayouts:

  def sectionLayout: GridLayoutSection => LayoutsMap = _ match {
    case GridLayoutSection.ProgramsLayout     => programs.defaultProgramsLayouts
    case GridLayoutSection.ConstraintsLayout  => constraints.defaultConstraintsLayouts
    case GridLayoutSection.SchedulingLayout   => scheduling.defaultSchedulingLayouts
    case GridLayoutSection.TargetLayout       => targets.defaultTargetLayouts
    case GridLayoutSection.ObservationsLayout => observations.defaultObsLayouts
    case GridLayoutSection.OverviewLayout     => overview.defaultOverviewLayouts
    case GridLayoutSection.ProposalLayout     => proposal.defaultProposalLayouts
    case GridLayoutSection.GroupEditLayout    => groupEdit.defaultGroupEditLayouts
  }

  val DefaultLayouts: Map[GridLayoutSection, LayoutsMap] =
    SortedMap.from(GridLayoutSection.values.map(l => l -> sectionLayout(l)))

  lazy val DefaultWidth: NonNegInt      = 10.refined
  lazy val DefaultLargeWidth: NonNegInt = 16.refined

  object constraints:
    private lazy val ConstraintsHeight: NonNegInt   = 4.refined
    private lazy val TimingWindowsHeight: NonNegInt = 14.refined

    private lazy val layoutMedium: Layout = Layout(
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

    lazy val defaultConstraintsLayouts = defineStdLayouts(
      Map(
        (BreakpointName.lg,
         layoutItems.andThen(layoutItemWidth).replace(DefaultLargeWidth)(layoutMedium)
        ),
        (BreakpointName.md, layoutMedium)
      )
    )

  object scheduling:
    private lazy val SchedulingHeight: NonNegInt = 14.refined

    private lazy val layoutMedium: Layout = Layout(
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

    lazy val defaultSchedulingLayouts = defineStdLayouts(
      Map(
        (BreakpointName.lg,
         layoutItems.andThen(layoutItemWidth).replace(DefaultLargeWidth)(layoutMedium)
        ),
        (BreakpointName.md, layoutMedium)
      )
    )

  object targets:
    private lazy val SummaryHeight: NonNegInt    = 6.refined
    private lazy val SummaryMinHeight: NonNegInt = 4.refined
    private lazy val TargetHeight: NonNegInt     = 18.refined
    private lazy val TargetMinHeight: NonNegInt  = 15.refined
    private lazy val SkyPlotHeight: NonNegInt    = 9.refined
    private lazy val SkyPlotMinHeight: NonNegInt = 6.refined
    private lazy val TileMinWidth: NonNegInt     = 5.refined

    private lazy val layoutMedium: Layout = Layout(
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

    lazy val defaultTargetLayouts = defineStdLayouts(
      Map(
        (BreakpointName.lg,
         layoutItems.andThen(layoutItemWidth).replace(DefaultLargeWidth)(layoutMedium)
        ),
        (BreakpointName.md, layoutMedium)
      )
    )

    private lazy val singleLayoutMedium: Layout = Layout(
      List(
        LayoutItem(
          i = ObsTabTilesIds.TargetSummaryId.id.value,
          x = 0,
          y = 0,
          w = DefaultWidth.value,
          h = 100, // This doesn't matter, we are forcing 100%.
          minW = TileMinWidth.value,
          static = true
        )
      )
    )

    lazy val defaultSingleLayouts: LayoutsMap = defineStdLayouts(
      Map(
        (BreakpointName.lg,
         layoutItems.andThen(layoutItemWidth).replace(DefaultLargeWidth)(singleLayoutMedium)
        ),
        (BreakpointName.md, singleLayoutMedium)
      )
    )

  object observations:
    private lazy val NotesMaxHeight: NonNegInt         = 5.refined
    private lazy val TargetHeight: NonNegInt           = 18.refined
    private lazy val TargetMinHeight: NonNegInt        = 15.refined
    private lazy val SkyPlotHeight: NonNegInt          = 9.refined
    private lazy val SkyPlotMinHeight: NonNegInt       = 6.refined
    private lazy val ConstraintsMinHeight: NonNegInt   = 4.refined
    private lazy val ConstraintsMaxHeight: NonNegInt   = 7.refined
    private lazy val SequenceMinHeight: NonNegInt      = 10.refined
    private lazy val SequenceMaxHeight: NonNegInt      = 14.refined
    private lazy val TimingWindowsMinHeight: NonNegInt = 8.refined
    private lazy val TimingWindowsMaxHeight: NonNegInt = 12.refined
    private lazy val ConfigurationMaxHeight: NonNegInt = 10.refined
    private lazy val ItcMaxHeight: NonNegInt           = 9.refined
    private lazy val FinderChartMinHeight: NonNegInt   = 6.refined
    private lazy val FinderChartHeight: NonNegInt      = 9.refined
    private lazy val TileMinWidth: NonNegInt           = 6.refined

    private lazy val layoutMedium: Layout = Layout(
      List(
        LayoutItem(
          x = 0,
          y = 0,
          w = DefaultWidth.value,
          h = NotesMaxHeight.value,
          i = ObsTabTilesIds.NotesId.id.value
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
        ),
        LayoutItem(
          x = 0,
          y =
            (NotesMaxHeight |+| TargetHeight |+| FinderChartHeight |+| SkyPlotHeight |+| ConstraintsMaxHeight |+| TimingWindowsMaxHeight |+| ConfigurationMaxHeight |+| ItcMaxHeight).value,
          w = DefaultWidth.value,
          h = SequenceMaxHeight.value,
          minH = SequenceMinHeight.value,
          i = ObsTabTilesIds.SequenceId.id.value
        )
      )
    )

    lazy val defaultObsLayouts: LayoutsMap =
      defineStdLayouts(
        Map(
          (BreakpointName.lg,
           layoutItems.andThen(layoutItemWidth).replace(DefaultLargeWidth)(layoutMedium)
          ),
          (BreakpointName.md, layoutMedium)
        )
      )

  object programs:
    private lazy val DetailsHeight: NonNegInt           = 6.refined
    private lazy val DetailsMinHeight: NonNegInt        = 4.refined
    private lazy val NotesHeight: NonNegInt             = 6.refined
    private lazy val NotesMinHeight: NonNegInt          = 4.refined
    private lazy val ChangeRequestsHeight: NonNegInt    = 6.refined
    private lazy val ChangeRequestsMinHeight: NonNegInt = 4.refined
    private lazy val TileMinWidth: NonNegInt            = 6.refined

    private lazy val layoutMedium: Layout = Layout(
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

    lazy val defaultProgramsLayouts = defineStdLayouts(
      Map(
        (BreakpointName.lg,
         layoutItems.andThen(layoutItemWidth).replace(DefaultLargeWidth)(layoutMedium)
        ),
        (BreakpointName.md, layoutMedium)
      )
    )

  object overview:

    private lazy val WarningsAndErrorsHeight: NonNegInt    = 8.refined
    private lazy val WarningsAndErrorsMinHeight: NonNegInt = 6.refined
    private lazy val ObsAttachmentsHeight: NonNegInt       = 8.refined
    private lazy val ObsAttachmentsMinHeight: NonNegInt    = 6.refined
    private lazy val TileMinWidth: NonNegInt               = 8.refined

    private lazy val layoutMedium: Layout = Layout(
      List(
        LayoutItem(
          i = ObsTabTilesIds.WarningsAndErrorsId.id.value,
          x = 0,
          y = 0,
          w = DefaultWidth.value,
          h = WarningsAndErrorsHeight.value,
          minH = WarningsAndErrorsMinHeight.value,
          minW = TileMinWidth.value
        ),
        LayoutItem(
          i = ObsTabTilesIds.ObsAttachmentsId.id.value,
          x = 0,
          y = WarningsAndErrorsHeight.value,
          w = DefaultWidth.value,
          h = ObsAttachmentsHeight.value,
          minH = ObsAttachmentsMinHeight.value,
          minW = TileMinWidth.value
        )
      )
    )

    lazy val defaultOverviewLayouts = defineStdLayouts(
      Map(
        (BreakpointName.lg,
         layoutItems.andThen(layoutItemWidth).replace(DefaultLargeWidth)(layoutMedium)
        ),
        (BreakpointName.md, layoutMedium)
      )
    )

  object proposal:
    private lazy val DetailsHeight: NonNegInt        = 7.refined
    private lazy val DetailsMinHeight: NonNegInt     = 1.refined
    private lazy val UsersHeight: NonNegInt          = 6.refined
    private lazy val UsersMinHeight: NonNegInt       = 4.refined
    private lazy val AbstractHeight: NonNegInt       = 8.refined
    private lazy val AbstractMinHeight: NonNegInt    = 4.refined
    private lazy val AttachmentsHeight: NonNegInt    = 8.refined
    private lazy val AttachmentsMinHeight: NonNegInt = 6.refined
    private lazy val TileMinWidth: NonNegInt         = 6.refined

    private lazy val layoutMedium: Layout = Layout(
      List(
        LayoutItem(
          i = ProposalTabTileIds.DetailsId.id.value,
          x = 0,
          y = 0,
          w = DefaultWidth.value,
          h = DetailsHeight.value,
          minH = DetailsMinHeight.value,
          minW = TileMinWidth.value
        ),
        LayoutItem(
          i = ProposalTabTileIds.UsersId.id.value,
          x = 0,
          y = 0,
          w = DefaultWidth.value,
          h = UsersHeight.value,
          minH = UsersMinHeight.value,
          minW = TileMinWidth.value
        ),
        LayoutItem(
          i = ProposalTabTileIds.AbstractId.id.value,
          x = 0,
          y = DetailsHeight.value,
          w = DefaultWidth.value,
          h = AbstractHeight.value,
          minH = AbstractMinHeight.value,
          minW = TileMinWidth.value
        ),
        LayoutItem(
          i = ProposalTabTileIds.AttachmentsId.id.value,
          x = 0,
          y = (DetailsHeight |+| AbstractHeight).value,
          w = DefaultWidth.value,
          h = AttachmentsHeight.value,
          minH = AttachmentsMinHeight.value,
          minW = TileMinWidth.value
        )
      )
    )

    lazy val defaultProposalLayouts = defineStdLayouts(
      Map(
        (BreakpointName.lg,
         layoutItems.andThen(layoutItemWidth).replace(DefaultLargeWidth)(layoutMedium)
        ),
        (BreakpointName.md, layoutMedium)
      )
    )

  object groupEdit:
    lazy val GroupEditHeight: NonNegInt    = 12.refined
    lazy val GroupEditMinHeight: NonNegInt = 6.refined
    lazy val NotesHeight: NonNegInt        = 8.refined
    lazy val NotesMinHeight: NonNegInt     = 4.refined
    lazy val TileMinWidth: NonNegInt       = 6.refined

    private lazy val layoutMedium: Layout = Layout(
      List(
        LayoutItem(
          i = GroupEditIds.GroupEditId.id.value,
          x = 0,
          y = 0,
          w = DefaultWidth.value,
          h = GroupEditHeight.value,
          minH = GroupEditMinHeight.value,
          minW = TileMinWidth.value
        ),
        LayoutItem(
          i = GroupEditIds.GroupNotesId.id.value,
          x = 0,
          y = GroupEditHeight.value,
          w = DefaultWidth.value,
          h = NotesHeight.value,
          minH = NotesMinHeight.value,
          minW = TileMinWidth.value
        )
      )
    )

    lazy val defaultGroupEditLayouts = defineStdLayouts(
      Map(
        (BreakpointName.lg,
         layoutItems.andThen(layoutItemWidth).replace(DefaultLargeWidth)(layoutMedium)
        ),
        (BreakpointName.md, layoutMedium)
      )
    )
