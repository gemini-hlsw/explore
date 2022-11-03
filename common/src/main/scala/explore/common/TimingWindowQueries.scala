// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import explore.model.TimingWindowEntry
import monocle.Focus
import monocle.Lens
import queries.common.TimingWindowsGQL.*
// import queries.schemas.odb.ODBConversions.*

object TimingQueries:

  type TimingWindowResult = TimingWindowsQuery.Data
  val TimingWindowsList: Lens[TimingWindowResult, List[TimingWindowEntry]] =
    Focus[TimingWindowResult](_.tmpTimingWindows)
