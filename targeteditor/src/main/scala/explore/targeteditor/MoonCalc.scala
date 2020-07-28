package explore.targeteditor

import java.time.Instant
import java.time.ZoneOffset
import java.time.ZonedDateTime
import java.time.Duration

object MoonCalc {
  val MeanLunationDays: Double    = 29.530588861
  val MeanLunationSeconds: Double = MeanLunationDays * 24 * 60 * 60
  val LunationZeroStart: Instant  =
    ZonedDateTime.of(2000, 1, 6, 18, 14, 0, 0, ZoneOffset.UTC).toInstant

  // Returns between 0.0 - 1.0, where 0.0 and 1.0 are a new moon, 0.5 a full moon
  def approxPhase(at: Instant): Double = {
    val partialPhase =
      (Duration.between(LunationZeroStart, at).getSeconds / MeanLunationSeconds) % 1.0
    if (partialPhase < 0) 1.0 + partialPhase else partialPhase
  }
}
