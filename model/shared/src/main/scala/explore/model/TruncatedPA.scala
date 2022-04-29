// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import lucuma.core.math.Angle
import spire.math.Rational

/**
 * A wrapper for Angle that is rounded to 2 decimal places of precision. This is used for input in
 * the UI to allow for lawful ValidFormatInput instances.
 *
 * @param posAangle
 *   The wrapped Angle. Guaranteed to have no more than 2 decimals of precision.
 */
sealed abstract case class TruncatedPA private (angle: Angle)

object TruncatedPA {
  def apply(angle: Angle): TruncatedPA = {
    val microSecs = Rational(angle.toMicroarcseconds, 100).round.toLong * 100
    new TruncatedPA(Angle.fromMicroarcseconds(microSecs)) {}
  }

  implicit val truncatedPAEq: Eq[TruncatedPA] = Eq.fromUniversalEquals
}
