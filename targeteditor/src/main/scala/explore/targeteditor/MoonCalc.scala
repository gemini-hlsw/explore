package explore.targeteditor

import gsp.math.JulianDate
import java.time.Instant
import java.time.ZoneOffset

object MoonCalc {
  sealed abstract class Phase(val constant: Double)
  object Phase {
    case object New          extends Phase(0.0)
    case object FirstQuarter extends Phase(0.25)
    case object Full         extends Phase(0.5)
    case object LastQuarter  extends Phase(0.75)
  }

  /**
    * Gives the JDE of the indicated phase of the moon.
    * This algorithm comes from pp 319-320 of "Astronomical Algorithms"
    * by Jean Meeus, ISBN 0-943396-35-2
    * This method does not apply certain corrections to the algorithm
    * that can amount to a large fraction of a day.
    * @param period Number of moon orbit periods since new moon of Jan 6, 2000.
    * Could be positive or negative.
    * @param phase desired phase
    * According to "Astronomical Algorithms" the period number can be estimated
    * by period = (year - 2000) * 12.3685
    * where year is a fractional year (e.g. mid feb 1977 = 1977.13)
    */

  def julianDateOfPhase(period: Int, phase: Phase): JulianDate = {
    val k = period + phase.constant
    val t = k / 1236.85
    JulianDate.fromDoubleApprox(
      2451550.09765 + 29.530588853 * k + 0.0001337 * t * t - 0.000000150 * t * t * t + 0.00000000073 * t * t * t * t
    )
  }

  /**
    * Estimates the lunar period relative to new moon of Jan 6, 2000.
    * This period can be used as an argument to julianDateOfPhase().
    */
  def approximatePeriod(instant: Instant): Int = {
    val zdt    = instant.atZone(ZoneOffset.UTC)
    val year   = zdt.getYear + zdt.getDayOfYear / 365.0
    val period = (year - 2000) * 12.3685
    Math // The original code performed a different adjustment here, but it sometimes returned the adjacent period.
      .floor(period)
      .toInt
  }

  /**
    * Returns the time of the specified phase of the moon for the
    * specified period.
    * @param period Number of moon orbit periods since new moon of Jan 6, 2000.
    * Could be positive or negative.
    * @param phase desired phase.
    * @return The time of the next phase as a timestamp
    */
  def getMoonTime(period: Int, phase: Phase): Instant =
    julianDateOfPhase(period, phase).toInstant

}
