// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import explore.model.TimingWindow
import explore.model.TimingWindowEntry
import monocle.Focus
import monocle.Iso
import monocle.Lens
import queries.common.TimingWindowsGQL.*
// import queries.schemas.odb.ODBConversions.*

object TimingQueries:

  type TimingWindowResult = TimingWindowsQuery.Data
  val TimingWindowResult = TimingWindowsQuery.Data

  val EntryToTimingWindows: Iso[List[TimingWindowResult.TmpTimingWindows], List[TimingWindow]] =
    Iso[List[TimingWindowResult.TmpTimingWindows], List[TimingWindow]](
      _.map(TimingWindowEntry.toTimingWindow)
    )(_.map(TimingWindowEntry.fromTimingWindow))

  val TimingWindowsList: Lens[TimingWindowResult, List[TimingWindow]] =
    Focus[TimingWindowResult](_.tmpTimingWindows).andThen(EntryToTimingWindows)
