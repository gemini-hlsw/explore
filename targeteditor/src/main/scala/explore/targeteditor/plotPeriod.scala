// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import lucuma.core.enum.Site
import lucuma.core.model.{ LocalObservingNight, ObservingNight, Semester }

import java.time.{ Duration, Instant, LocalDate }

sealed trait PlotPeriod {
  def start(site: Site): Instant
  def end(site:   Site): Instant
  val every: Duration
  val dateTimeFomat: String
}

object PlotPeriod {
  final case class NightPlot(date: LocalDate) extends PlotPeriod {
    // Change this to TwilightBoundedNight when we have it
    private val lon: LocalObservingNight = LocalObservingNight(date)

    def start(site: Site): Instant = ObservingNight(site, lon).start
    def end(site:   Site): Instant = ObservingNight(site, lon).end
    override val every: Duration       = Duration.ofMinutes(1)
    override val dateTimeFomat: String = "%H:%M"
  }
  final case class SemesterPlot(semester: Semester) extends PlotPeriod {
    // This may not be quite right. How do we plot for a semester?
    def start(site: Site): Instant = semester.start.atSite(site).toInstant
    def end(site:   Site): Instant = semester.end.atSite(site).toInstant
    override val every: Duration       = Duration.ofHours(6)
    override val dateTimeFomat: String = "%Y-%m-%d"
  }
}
