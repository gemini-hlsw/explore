// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.plots

import java.time.Duration
import java.time.Instant
import java.time.ZoneOffset
import java.time.ZonedDateTime

object MoonCalc:
  val MeanLunationDays: Double    = 29.530588861
  val MeanLunationSeconds: Double = MeanLunationDays * 24 * 60 * 60
  val LunationZeroStart: Instant  =
    ZonedDateTime.of(2000, 1, 6, 18, 14, 0, 0, ZoneOffset.UTC).toInstant

  /**
   * Fast, linear moon phase approximation based on mean lunation period.
   *
   * Good enough to display moon phase icon.
   *
   * For exact calculation, see "Astronomical Algorithms, 2nd. ed" by Jean Meeus, ISBN
   * 0-943396-61-1, Chapter 49. For other approximations, see http://www.ben-daglish.net/moon.shtml.
   *
   * @return
   *   A value between 0.0 - 1.0, where 0.0 and 1.0 are a new moon, 0.5 a full moon.
   */
  def approxPhase(at: Instant): Double = {
    val partialPhase =
      (Duration.between(LunationZeroStart, at).getSeconds / MeanLunationSeconds) % 1.0
    if (partialPhase < 0) 1.0 + partialPhase else partialPhase
  }
