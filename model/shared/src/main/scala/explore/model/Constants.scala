// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.math.Angle
import lucuma.core.model.UnnormalizedSED

import java.time.Duration
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter

trait Constants:
  val TwoPanelCutoff           = 576.0
  val InitialTreeWidth         = 300.0
  val MinLeftPanelWidth        = 270.0
  val GridRowHeight            = 36
  val GridRowPadding           = 5
  val GridColCount             = 12
  // 1 arcmin
  val PreviewFov: Angle        = Angle.fromMicroarcseconds(60000000L)
  val InitialFov: Angle        = PreviewFov
  val SimbadResultLimit        = 50
  val MaxConcurrentItcRequests = 4
  val DefaultSED               = UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.O5V)

  val GppDateFormatter: DateTimeFormatter   = DateTimeFormatter.ofPattern("yyyy-MMM-dd")
  val GppTimeFormatter: DateTimeFormatter   = DateTimeFormatter.ofPattern("HH:mm")
  val GppTimeTZFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm z")
  val IsoUTCFormatter: DateTimeFormatter    =
    DateTimeFormatter.ISO_OFFSET_DATE_TIME.withZone(ZoneOffset.UTC)
  val UtcFormatter: DateTimeFormatter       =
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").withZone(ZoneOffset.UTC)
  val DurationFormatter: Duration => String = d =>
    val hours: Option[Long]  = d.toHours.some.filter(_ > 0)
    val minutes: Option[Int] = d.toMinutesPart.some.filter(_ > 0 || hours.isDefined)
    val seconds: Int         = d.toSecondsPart

    hours.map(h => s"${h}h").orEmpty + minutes.map(m => s"${m}m").orEmpty + s"${seconds}s"

  val NoGuideStarMessage = "No guidestar available"

object Constants extends Constants
