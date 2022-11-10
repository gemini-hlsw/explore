// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import lucuma.core.math.Angle

import java.time.ZoneId
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter

trait Constants {
  val TwoPanelCutoff                   = 576.0
  val InitialTreeWidth                 = 300.0
  val MinLeftPanelWidth                = 270.0
  val GridRowHeight                    = 36
  val GridRowPadding                   = 5
  val GridColCount                     = 12
  val InitialFov: Angle                = Angle.fromDoubleDegrees(0.25)
  val AngleSizeFovFactor: Long => Long = v => (v * 3) / 2
  val SimbadResultLimit                = 50
  val MaxConcurrentItcRequests         = 4

  val UTC       = ZoneId.of("UTC")
  val UTCOffset = ZoneOffset.UTC

  val GppDateFormatter: DateTimeFormatter   = DateTimeFormatter.ofPattern("yyyy-MMM-dd")
  val GppTimeFormatter: DateTimeFormatter   = DateTimeFormatter.ofPattern("HH:mm")
  val GppTimeTZFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm z")
  val IsoUTCFormatter: DateTimeFormatter    =
    DateTimeFormatter.ISO_OFFSET_DATE_TIME.withZone(Constants.UTC)
}

object Constants extends Constants
