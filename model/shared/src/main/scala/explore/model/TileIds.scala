// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import eu.timepit.refined.types.string.NonEmptyString
import lucuma.refined.*

enum ObsTabTilesIds:
  case NotesId, TargetSummaryId, TargetId, PlotId, PlotGNId, PlotGSId, ConstraintsId,
    ConfigurationId, ItcId, TimingWindowsId, WarningsAndErrorsId, ObsAttachmentsId, FinderChartsId,
    SequenceId

  def id: NonEmptyString = this match
    case NotesId             => "notes".refined
    case TargetSummaryId     => "targetSummary".refined
    case TargetId            => "target".refined
    case PlotId              => "elevationPlot".refined
    case PlotGNId            => "elevationPlotGN".refined
    case PlotGSId            => "elevationPlotGS".refined
    case ConstraintsId       => "constraints".refined
    case ConfigurationId     => "configuration".refined
    case ItcId               => "itc".refined
    case TimingWindowsId     => "timingWindows".refined
    case WarningsAndErrorsId => "warningsAndErrors".refined
    case ObsAttachmentsId    => "obsAttachments".refined
    case FinderChartsId      => "finderCharts".refined
    case SequenceId          => "sequence".refined

enum ProgramTabTileIds:
  case DetailsId, NotesId, ChangeRequestsId

  def id: NonEmptyString = this match
    case DetailsId        => "programDetails".refined
    case NotesId          => "programNotes".refined
    case ChangeRequestsId => "programChangeRequests".refined

enum ProposalTabTileIds:
  case DetailsId, UsersId, AbstractId, AttachmentsId

  def id: NonEmptyString = this match
    case DetailsId     => "proposalDetails".refined
    case UsersId       => "proposalUsers".refined
    case AbstractId    => "proposalAbstract".refined
    case AttachmentsId => "proposalAttachments".refined

enum GroupEditIds:
  case GroupEditId, GroupNotesId

  def id: NonEmptyString = this match
    case GroupEditId  => "groupEdit".refined
    case GroupNotesId => "groupNotes".refined

enum TargetTabControllerIds(val id: NonEmptyString):
  case Summary        extends TargetTabControllerIds("target-summary-controller".refined)
  case AsterismEditor extends TargetTabControllerIds("target-obs-controller".refined)
