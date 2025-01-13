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
import lucuma.core.enums.CalibrationRole
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
    case GridLayoutSection.ProgramsLayout              => programs.defaultProgramsLayouts
    case GridLayoutSection.ConstraintsLayout           => constraints.defaultConstraintsLayouts
    case GridLayoutSection.SchedulingLayout            => scheduling.defaultSchedulingLayouts
    case GridLayoutSection.TargetLayout                => targets.defaultTargetLayouts
    case GridLayoutSection.ObservationsLayout          => observations.defaultObsLayouts
    case GridLayoutSection.ObservationsSpecPhotoLayout => observations.specPhotoObsLayouts
    case GridLayoutSection.ObservationsTwilightLayout  => observations.twilightObsLayouts
    case GridLayoutSection.ObservationsSequenceLayout  => observations.sequenceObsLayouts
    case GridLayoutSection.ObservationListLayout       => observationList.defaultObsListLayouts
    case GridLayoutSection.OverviewLayout              => overview.defaultOverviewLayouts
    case GridLayoutSection.ProposalLayout              => proposal.defaultProposalLayouts
    case GridLayoutSection.GroupEditLayout             => groupEdit.defaultGroupEditLayouts
  }

  extension (l: LayoutsMap)
    def withMinWidth: LayoutsMap =
      l.map {
        case (BreakpointName.lg, v) =>
          BreakpointName.lg -> (v.copy(_3 =
            Layout(v._3.asList.map(_.copy(w = DefaultLargeWidth.value, minW = TileMinWidth.value)))
          ): LayoutEntry)
        case (BreakpointName.md, v) =>
          BreakpointName.md -> (v.copy(_3 =
            Layout(v._3.asList.map(_.copy(w = DefaultWidth.value, minW = TileMinWidth.value)))
          ): LayoutEntry)
      }

  val DefaultLayouts: Map[GridLayoutSection, LayoutsMap] =
    SortedMap.from(GridLayoutSection.values.map(l => l -> sectionLayout(l)))

  private lazy val DefaultWidth: NonNegInt      = 16.refined
  private lazy val DefaultLargeWidth: NonNegInt = 32.refined

  // Restricted to GridRowHeight * 10 = 360px which is basically the min width for a mobile device
  private lazy val TileMinWidth: NonNegInt = 8.refined

  object constraints:
    private lazy val ConstraintsHeight: NonNegInt   = 4.refined
    private lazy val TimingWindowsHeight: NonNegInt = 14.refined

    private lazy val layoutMedium: Layout = Layout(
      List(
        LayoutItem(
          i = ObsTabTileIds.ConstraintsId.id.value,
          x = 0,
          y = 0,
          w = DefaultWidth.value,
          h = ConstraintsHeight.value
        ),
        LayoutItem(
          i = ObsTabTileIds.TimingWindowsId.id.value,
          x = 0,
          y = ConstraintsHeight.value,
          w = DefaultWidth.value,
          h = TimingWindowsHeight.value
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
    ).withMinWidth
  end constraints

  object scheduling:
    private lazy val SchedulingHeight: NonNegInt = 14.refined

    private lazy val layoutMedium: Layout = Layout(
      List(
        LayoutItem(
          i = ObsTabTileIds.TimingWindowsId.id.value,
          x = 0,
          y = 0,
          w = DefaultWidth.value,
          h = SchedulingHeight.value
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
    ).withMinWidth
  end scheduling

  object targets:
    private lazy val SummaryHeight: NonNegInt = 9.refined
    private lazy val TargetHeight: NonNegInt  = 18.refined
    private lazy val SkyPlotHeight: NonNegInt = 9.refined

    private lazy val layoutMedium: Layout = Layout(
      List(
        LayoutItem(
          i = TargetTabTileIds.Summary.id,
          x = 0,
          y = 0,
          w = DefaultWidth.value,
          h = SummaryHeight.value
        ),
        LayoutItem(
          i = TargetTabTileIds.AsterismEditor.id,
          x = 0,
          y = SummaryHeight.value,
          w = DefaultWidth.value,
          h = TargetHeight.value
        ),
        LayoutItem(
          i = TargetTabTileIds.ElevationPlot.id,
          x = 0,
          y = SummaryHeight.value + TargetHeight.value,
          w = DefaultWidth.value,
          h = SkyPlotHeight.value
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
          i = ObsTabTileIds.TargetSummaryId.id.value,
          x = 0,
          y = 0,
          w = DefaultWidth.value,
          h = 100, // This doesn't matter, we are forcing 100%.
          static = true
        )
      )
    )

    lazy val defaultSingleLayouts = defineStdLayouts(
      Map(
        (BreakpointName.lg,
         layoutItems.andThen(layoutItemWidth).replace(DefaultLargeWidth)(singleLayoutMedium)
        ),
        (BreakpointName.md, singleLayoutMedium)
      )
    ).withMinWidth
  end targets

  object observations:
    private lazy val NotesMaxHeight: NonNegInt         = 5.refined
    private lazy val TargetHeight: NonNegInt           = 18.refined
    private lazy val SkyPlotHeight: NonNegInt          = 9.refined
    private lazy val ConstraintsMaxHeight: NonNegInt   = 7.refined
    private lazy val SequenceMaxHeight: NonNegInt      = 14.refined
    private lazy val TimingWindowsMaxHeight: NonNegInt = 12.refined
    private lazy val ConfigurationMaxHeight: NonNegInt = 10.refined
    private lazy val ItcMaxHeight: NonNegInt           = 9.refined
    private lazy val FinderChartHeight: NonNegInt      = 9.refined

    private lazy val layoutMedium: Layout = Layout(
      List(
        LayoutItem(
          x = 0,
          y = 0,
          w = DefaultWidth.value,
          h = NotesMaxHeight.value,
          i = ObsTabTileIds.NotesId.id.value
        ),
        LayoutItem(
          x = 0,
          y = NotesMaxHeight.value,
          w = DefaultWidth.value,
          h = TargetHeight.value,
          i = ObsTabTileIds.TargetId.id.value
        ),
        LayoutItem(
          x = 0,
          y = (NotesMaxHeight |+| TargetHeight).value,
          w = DefaultWidth.value,
          h = FinderChartHeight.value,
          i = ObsTabTileIds.FinderChartsId.id.value
        ),
        LayoutItem(
          x = 0,
          y = (NotesMaxHeight |+| TargetHeight |+| FinderChartHeight).value,
          w = DefaultWidth.value,
          h = SkyPlotHeight.value,
          i = ObsTabTileIds.PlotId.id.value
        ),
        LayoutItem(
          x = 0,
          y = (NotesMaxHeight |+| TargetHeight |+| FinderChartHeight |+| SkyPlotHeight).value,
          w = DefaultWidth.value,
          h = ConstraintsMaxHeight.value,
          i = ObsTabTileIds.ConstraintsId.id.value
        ),
        LayoutItem(
          x = 0,
          y =
            (NotesMaxHeight |+| TargetHeight |+| FinderChartHeight |+| SkyPlotHeight |+| ConstraintsMaxHeight).value,
          w = DefaultWidth.value,
          h = TimingWindowsMaxHeight.value,
          i = ObsTabTileIds.TimingWindowsId.id.value
        ),
        LayoutItem(
          x = 0,
          y =
            (NotesMaxHeight |+| TargetHeight |+| FinderChartHeight |+| SkyPlotHeight |+| ConstraintsMaxHeight |+| TimingWindowsMaxHeight).value,
          w = DefaultWidth.value,
          h = ConfigurationMaxHeight.value,
          i = ObsTabTileIds.ConfigurationId.id.value
        ),
        LayoutItem(
          x = 0,
          y =
            (NotesMaxHeight |+| TargetHeight |+| FinderChartHeight |+| SkyPlotHeight |+| ConstraintsMaxHeight |+| TimingWindowsMaxHeight |+| ConfigurationMaxHeight).value,
          w = DefaultWidth.value,
          h = ItcMaxHeight.value,
          i = ObsTabTileIds.ItcId.id.value
        )
      )
    )

    lazy val twilightRemovedIds =
      List(ObsTabTileIds.FinderChartsId,
           ObsTabTileIds.ItcId,
           ObsTabTileIds.NotesId,
           ObsTabTileIds.TimingWindowsId
      ).map(_.id.value)

    lazy val twilightMedium      =
      layoutMedium.asList
        .filterNot(l => twilightRemovedIds.contains(l.i))
    lazy val specPhotoRemovedIds =
      List(ObsTabTileIds.FinderChartsId, ObsTabTileIds.NotesId, ObsTabTileIds.TimingWindowsId).map(
        _.id.value
      )
    lazy val specPhotoMedium     =
      layoutMedium.asList
        .filterNot(l => specPhotoRemovedIds.contains(l.i))

    lazy val sequenceMedium: Layout = Layout(
      List(
        LayoutItem(
          x = 0,
          y =
            (NotesMaxHeight |+| TargetHeight |+| FinderChartHeight |+| SkyPlotHeight |+| ConstraintsMaxHeight |+| TimingWindowsMaxHeight |+| ConfigurationMaxHeight |+| ItcMaxHeight).value,
          w = DefaultWidth.value,
          h = SequenceMaxHeight.value,
          i = ObsTabTileIds.SequenceId.id.value
        )
      )
    )

    def removedTiles(role: Option[CalibrationRole]) =
      role match
        case Some(CalibrationRole.Twilight)           => twilightRemovedIds
        case Some(CalibrationRole.SpectroPhotometric) => specPhotoRemovedIds
        case _                                        => Nil

    lazy val defaultObsLayouts: LayoutsMap =
      defineStdLayouts(
        Map(
          (BreakpointName.lg, layoutMedium),
          (BreakpointName.md, layoutMedium)
        )
      ).withMinWidth

    lazy val specPhotoObsLayouts: LayoutsMap =
      defineStdLayouts(
        Map(
          (BreakpointName.lg, Layout(specPhotoMedium)),
          (BreakpointName.md, Layout(specPhotoMedium))
        )
      ).withMinWidth

    lazy val twilightObsLayouts: LayoutsMap =
      defineStdLayouts(
        Map(
          (BreakpointName.lg, Layout(twilightMedium)),
          (BreakpointName.md, Layout(twilightMedium))
        )
      ).withMinWidth

    lazy val sequenceObsLayouts: LayoutsMap =
      defineStdLayouts(
        Map(
          (BreakpointName.lg, sequenceMedium),
          (BreakpointName.md, sequenceMedium)
        )
      )
  end observations

  object observationList:
    private lazy val SummaryHeight: NonNegInt = 9.refined
    private lazy val SkyPlotHeight: NonNegInt = 9.refined

    private lazy val layoutMedium: Layout = Layout(
      List(
        LayoutItem(
          i = ObsSummaryTabTileIds.SummaryId.id.value,
          x = 0,
          y = 0,
          w = DefaultWidth.value,
          h = SummaryHeight.value
        ),
        LayoutItem(
          i = ObsSummaryTabTileIds.PlotId.id.value,
          x = 0,
          y = SummaryHeight.value,
          w = DefaultWidth.value,
          h = SkyPlotHeight.value
        )
      )
    )

    lazy val defaultObsListLayouts: LayoutsMap =
      defineStdLayouts(
        Map(
          (BreakpointName.lg,
           layoutItems.andThen(layoutItemWidth).replace(DefaultLargeWidth)(layoutMedium)
          ),
          (BreakpointName.md, layoutMedium)
        )
      ).withMinWidth
  end observationList

  object programs:
    private lazy val DetailsHeight: NonNegInt            = 6.refined
    private lazy val NotesHeight: NonNegInt              = 6.refined
    private lazy val ChangeRequestsHeight: NonNegInt     = 6.refined
    private lazy val UnrequestedConfigsHeight: NonNegInt = 6.refined

    private lazy val layoutMedium: Layout = Layout(
      List(
        LayoutItem(
          i = ProgramTabTileIds.DetailsId.id.value,
          x = 0,
          y = 0,
          w = DefaultWidth.value,
          h = DetailsHeight.value
        ),
        LayoutItem(
          i = ProgramTabTileIds.NotesId.id.value,
          x = 0,
          y = DetailsHeight.value,
          w = DefaultWidth.value,
          h = NotesHeight.value
        ),
        LayoutItem(
          i = ProgramTabTileIds.ChangeRequestsId.id.value,
          x = 0,
          y = (DetailsHeight |+| NotesHeight).value,
          w = DefaultWidth.value,
          h = ChangeRequestsHeight.value
        ),
        LayoutItem(
          i = ProgramTabTileIds.UnrequestedConfigsId.id.value,
          x = 0,
          y = (DetailsHeight |+| NotesHeight |+| ChangeRequestsHeight).value,
          w = DefaultWidth.value,
          h = UnrequestedConfigsHeight.value
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
    ).withMinWidth
  end programs

  object overview:

    private lazy val WarningsAndErrorsHeight: NonNegInt = 8.refined
    private lazy val ObsAttachmentsHeight: NonNegInt    = 8.refined

    private lazy val layoutMedium: Layout = Layout(
      List(
        LayoutItem(
          i = ObsTabTileIds.WarningsAndErrorsId.id.value,
          x = 0,
          y = 0,
          w = DefaultWidth.value,
          h = WarningsAndErrorsHeight.value
        ),
        LayoutItem(
          i = ObsTabTileIds.ObsAttachmentsId.id.value,
          x = 0,
          y = WarningsAndErrorsHeight.value,
          w = DefaultWidth.value,
          h = ObsAttachmentsHeight.value
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
    ).withMinWidth
  end overview

  object proposal:
    private lazy val DetailsHeight: NonNegInt     = 7.refined
    private lazy val UsersHeight: NonNegInt       = 6.refined
    private lazy val AbstractHeight: NonNegInt    = 8.refined
    private lazy val AttachmentsHeight: NonNegInt = 8.refined

    private lazy val layoutMedium: Layout = Layout(
      List(
        LayoutItem(
          i = ProposalTabTileIds.DetailsId.id.value,
          x = 0,
          y = 0,
          w = DefaultWidth.value,
          h = DetailsHeight.value
        ),
        LayoutItem(
          i = ProposalTabTileIds.UsersId.id.value,
          x = 0,
          y = 0,
          w = DefaultWidth.value,
          h = UsersHeight.value
        ),
        LayoutItem(
          i = ProposalTabTileIds.AbstractId.id.value,
          x = 0,
          y = DetailsHeight.value,
          w = DefaultWidth.value,
          h = AbstractHeight.value
        ),
        LayoutItem(
          i = ProposalTabTileIds.AttachmentsId.id.value,
          x = 0,
          y = (DetailsHeight |+| AbstractHeight).value,
          w = DefaultWidth.value,
          h = AttachmentsHeight.value
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
    ).withMinWidth
  end proposal

  object groupEdit:
    lazy val GroupEditHeight: NonNegInt    = 12.refined
    lazy val GroupEditMinHeight: NonNegInt = 6.refined
    lazy val NotesHeight: NonNegInt        = 8.refined
    lazy val NotesMinHeight: NonNegInt     = 4.refined
    lazy val TileMinWidth: NonNegInt       = 6.refined

    private lazy val layoutMedium: Layout = Layout(
      List(
        LayoutItem(
          i = GroupEditTileIds.GroupEditId.id.value,
          x = 0,
          y = 0,
          w = DefaultWidth.value,
          h = GroupEditHeight.value
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
    ).withMinWidth
  end groupEdit
