// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor.plots

import cats.*
import cats.derived.*
import cats.syntax.all.*
import explore.model.ElevationPlotScheduling
import explore.model.enums.PlotRange
import explore.model.enums.TimeDisplay
import japgolly.scalajs.react.ReactCats.*
import japgolly.scalajs.react.Reusability
import lucuma.core.enums.Site
import lucuma.core.enums.TwilightType
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Coordinates
import lucuma.core.model.CoordinatesAtVizTime
import lucuma.core.model.ObjectTracking
import lucuma.core.model.ObservingNight
import lucuma.core.model.Semester
import monocle.Focus
import org.typelevel.cats.time.given

import java.time.Instant
import java.time.LocalDate

case class ObjectPlotOptions(
  site:           Site,
  range:          PlotRange,
  date:           LocalDate,
  semester:       Semester,
  timeDisplay:    TimeDisplay,
  showScheduling: ElevationPlotScheduling,
  visiblePlots:   List[SeriesType] = SeriesType.values.toList
) derives Eq:
  def withDateAndSemesterOf(observationTime: Instant): ObjectPlotOptions =
    val (date, semester) = ObjectPlotOptions.dateAndSemesterOf(observationTime.some, site)
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

object ObjectPlotOptions:
  val site           = Focus[ObjectPlotOptions](_.site)
  val range          = Focus[ObjectPlotOptions](_.range)
  val date           = Focus[ObjectPlotOptions](_.date)
  val semester       = Focus[ObjectPlotOptions](_.semester)
  val timeDisplay    = Focus[ObjectPlotOptions](_.timeDisplay)
  val showScheduling = Focus[ObjectPlotOptions](_.showScheduling)
  val visiblePlots   = Focus[ObjectPlotOptions](_.visiblePlots)

  private def dateAndSemesterOf(
    observationTime: Option[Instant],
    site:            Site
  ): (LocalDate, Semester) =
    val date: LocalDate =
      ObservingNight
        .fromSiteAndInstant(site, observationTime.getOrElse(Instant.now))
        .toLocalDate
    // if `fromLocalDate` returns None, date is out of range, so clamp
    // semester to the Min and Max semesters
    val semester        = Semester.fromLocalDate(date).getOrElse {
      if (date < Semester.MinValue.start.localDate) Semester.MinValue
      else Semester.MaxValue
    }
    (date, semester)

  def default(
    predefinedSite:  Option[Site],
    observationTime: Option[Instant],
    tracking:        ObjectTracking
  ) =
    val coords: Coordinates =
      observationTime.flatMap(tracking.at(_).map(_.value)).getOrElse(tracking.baseCoordinates)
    val site: Site          = predefinedSite.getOrElse(
      if (coords.dec.toAngle.toSignedDoubleDegrees > -5) Site.GN else Site.GS
    )
    val (date, semester)    = dateAndSemesterOf(observationTime, site)

    ObjectPlotOptions(
      site,
      PlotRange.Night,
      date,
      semester,
      TimeDisplay.Site,
      ElevationPlotScheduling.On,
      List(SeriesType.Elevation, SeriesType.SkyBrightness)
    )

  given Reusability[ObjectPlotOptions] = Reusability.byEq
