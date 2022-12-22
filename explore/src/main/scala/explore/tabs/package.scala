// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import eu.timepit.refined.types.string.NonEmptyString
import lucuma.refined.*

enum ObsTabTilesIds:
  case NotesId, TargetSummaryId, TargetId, PlotId, ConstraintsId, ConfigurationId, ItcId,
    TimingWindowsId

  def id: NonEmptyString = this match
    case NotesId         => "notes".refined
    case TargetSummaryId => "targetSummary".refined
    case TargetId        => "target".refined
    case PlotId          => "elevationPlot".refined
    case ConstraintsId   => "constraints".refined
    case ConfigurationId => "configuration".refined
    case ItcId           => "itc".refined
    case TimingWindowsId => "timingWindows".refined
