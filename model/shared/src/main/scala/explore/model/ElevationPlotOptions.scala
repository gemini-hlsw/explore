// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.*
import cats.derived.*
import cats.syntax.option.given
import explore.model.enums.PlotRange
import explore.model.enums.TimeDisplay
import lucuma.core.enums.Site
import lucuma.core.enums.TwilightType
import lucuma.core.math.BoundedInterval
import lucuma.core.model.CoordinatesAtVizTime
import lucuma.core.model.ObservingNight
import lucuma.core.model.Semester
import monocle.Focus
import org.typelevel.cats.time.given

import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.ZonedDateTime

case class ElevationPlotOptions(
  site:        Site,
  range:       PlotRange,
  date:        LocalDate,
  semester:    Semester,
  timeDisplay: TimeDisplay
) derives Eq:
  def withDateAndSemesterOf(visualizationTime: Instant): ElevationPlotOptions =
    val (date, semester) = ElevationPlotOptions.dateAndSemesterOf(visualizationTime.some, site)
    copy(date = date, semester = semester)

  def minInstant: Instant =
    range match
      case PlotRange.Night    =>
        ObservingNight
          .fromSiteAndLocalDate(site, date)
          .twilightBoundedUnsafe(TwilightType.Official)
          .start
      case PlotRange.Semester => semester.start.atSite(site).toInstant

  def maxInstant: Instant =
    range match
      case PlotRange.Night    =>
        ObservingNight
          .fromSiteAndLocalDate(site, date)
          .twilightBoundedUnsafe(TwilightType.Official)
          .`end`
      case PlotRange.Semester => semester.`end`.atSite(site).toInstant

  def interval: BoundedInterval[Instant] = BoundedInterval.unsafeClosed(minInstant, maxInstant)

object ElevationPlotOptions:
  val site        = Focus[ElevationPlotOptions](_.site)
  val range       = Focus[ElevationPlotOptions](_.range)
  val date        = Focus[ElevationPlotOptions](_.date)
  val semester    = Focus[ElevationPlotOptions](_.semester)
  val timeDisplay = Focus[ElevationPlotOptions](_.timeDisplay)

  private def dateAndSemesterOf(
    visualizationTime: Option[Instant],
    site:              Site
  ): (LocalDate, Semester) =
    val date: LocalDate =
      ObservingNight
        .fromSiteAndInstant(site, visualizationTime.getOrElse(Instant.now))
        .toLocalDate
    (date, Semester.fromLocalDate(date))

  def default(
    predefinedSite:    Option[Site],
    visualizationTime: Option[Instant],
    coords:            CoordinatesAtVizTime
  ) =
    val site: Site       = predefinedSite.getOrElse(
      if (coords.value.dec.toAngle.toSignedDoubleDegrees > -5) Site.GN else Site.GS
    )
    val (date, semester) = dateAndSemesterOf(visualizationTime, site)

    ElevationPlotOptions(
      site,
      PlotRange.Night,
      date,
      Semester.fromLocalDate(date),
      TimeDisplay.Site
    )
