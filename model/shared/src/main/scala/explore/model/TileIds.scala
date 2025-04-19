// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import eu.timepit.refined.types.string.NonEmptyString
import lucuma.refined.*

enum ObsTabTileIds:
  case NotesId, TargetSummaryId, TargetId, PlotId, ConstraintsId, ConfigurationId, ItcId,
    TimingWindowsId, FinderChartsId, SequenceId

  def id: NonEmptyString = this match
    case NotesId         => "notes".refined
    case TargetSummaryId => "targetSummary".refined
    case TargetId        => "target".refined
    case PlotId          => "elevationPlot".refined
    case ConstraintsId   => "constraints".refined
    case ConfigurationId => "configuration".refined
    case ItcId           => "itc".refined
    case TimingWindowsId => "timingWindows".refined
    case FinderChartsId  => "finderCharts".refined
    case SequenceId      => "sequence".refined

enum ObsSummaryTabTileIds:
  case SummaryId, PlotId

  def id: NonEmptyString = this match
    case SummaryId => "summary".refined
    case PlotId    => "plot".refined

enum ProgramTabTileIds:
  case DetailsId, NotesId, ChangeRequestsId, UnrequestedConfigsId, DataUsers

  def id: NonEmptyString = this match
    case DetailsId            => "programDetails".refined
    case NotesId              => "programNotes".refined
    case ChangeRequestsId     => "programChangeRequests".refined
    case UnrequestedConfigsId => "unrequestedConfigs".refined
    case DataUsers            => "dataUsers".refined

enum ProposalTabTileIds:
  case DetailsId, UsersId, AbstractId, AttachmentsId

  def id: NonEmptyString = this match
    case DetailsId     => "proposalDetails".refined
    case UsersId       => "proposalUsers".refined
    case AbstractId    => "proposalAbstract".refined
    case AttachmentsId => "proposalAttachments".refined

enum GroupEditTileIds:
  case GroupEditId

  def id: NonEmptyString = this match
    case GroupEditId => "groupEdit".refined

enum TargetTabTileIds(val id: NonEmptyString):
  case Summary        extends TargetTabTileIds("targetSummary".refined)
  case AsterismEditor extends TargetTabTileIds("targetEditor".refined)
  case ElevationPlot  extends TargetTabTileIds("targetPlot".refined)

enum ConstraintTabTileIds(val id: NonEmptyString):
  case Summary extends ConstraintTabTileIds("constraints".refined)

enum OverviewTabTileIds(val id: NonEmptyString):
  case WarningsAndErrorsId extends OverviewTabTileIds("warningsAndErrors".refined)
  case GroupWarningsId     extends OverviewTabTileIds("groupWarnings".refined)
  case AttachmentsId       extends OverviewTabTileIds("attachments".refined)
  case DescriptionId       extends OverviewTabTileIds("programDescription".refined)
