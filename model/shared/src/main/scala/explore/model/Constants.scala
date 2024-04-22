// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import lucuma.core.math.Angle

import java.time.Duration
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter

trait Constants:
  val UTC                      = ZoneOffset.UTC
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

  val GppDateFormatter: DateTimeFormatter           = DateTimeFormatter.ofPattern("yyyy-MMM-dd")
  val GppTimeFormatter: DateTimeFormatter           = DateTimeFormatter.ofPattern("HH:mm")
  val GppTimeTZFormatter: DateTimeFormatter         =
    DateTimeFormatter.ofPattern("HH:mm").withZone(ZoneOffset.UTC)
  val GppTimeTZFormatterWithZone: DateTimeFormatter =
    DateTimeFormatter.ofPattern("HH:mm 'UTC'").withZone(ZoneOffset.UTC)
  val IsoUTCFormatter: DateTimeFormatter            =
    DateTimeFormatter.ISO_OFFSET_DATE_TIME.withZone(ZoneOffset.UTC)
  val UtcFormatter: DateTimeFormatter               =
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").withZone(ZoneOffset.UTC)
  val DurationFormatter: Duration => String         = d =>
    val hours: Option[Long]  = d.toHours.some.filter(_ > 0)
    val minutes: Option[Int] = d.toMinutesPart.some.filter(_ > 0 || hours.isDefined)
    val seconds: Int         = d.toSecondsPart
    hours.map(h => s"${h}h").orEmpty + minutes.map(m => s"${m}m").orEmpty + s"${seconds}s"
  val DurationLongFormatter: Duration => String     = d =>
    val days: Option[Long]   = d.toDays.some.filter(_ > 0)
    val hours: Option[Int]   = d.toHoursPart.some.filter(_ > 0)
    val minutes: Option[Int] = d.toMinutesPart.some.filter(_ > 0 || (days.isEmpty && hours.isEmpty))
    List(days, hours, minutes)
      .zip(List("day", "hour", "minute"))
      .map((nOpt, units) => nOpt.map(n => s"$n $units" + (if (n != 1) "s" else "")))
      .flattenOption
      .mkString(", ")

  val NoGuideStarMessage = "No guidestar available"
  val MissingITC         = "Mode or ITC not available"
  val MissingCandidates  = "No catalog stars available"
  val BadTimingWindow    = "Review the dates on this timing window."
  val MissingInfoMsg     = "Not enough information to call ITC"

object Constants extends Constants
