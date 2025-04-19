// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.syntax.all.*
import coulomb.*
import coulomb.syntax.*
import coulomb.units.accepted.*
import eu.timepit.refined.api.*
import eu.timepit.refined.numeric.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.refined.*

case class Progress private (current: NonNegInt, total: NonNegInt) {
  lazy val percentage: Quantity[Progress.PercentRange, Percent] =
    Progress.PercentRange
      .unsafeFrom(current.value * 100.0 / total.value)
      .withUnit[Percent]

  val complete: Boolean = current.value === total.value

  def increment(steps: NonNegInt = 1.refined): Progress =
    if (current.value + steps.value > total.value)
      Progress(NonNegInt.unsafeFrom(total.value), total)
    else
      Progress(NonNegInt.unsafeFrom(current.value + steps.value), total)
}

object Progress {
  type PercentRange = Double Refined Interval.Closed[0.0, 100.0]
  object PercentRange extends RefinedTypeOps.Numeric[PercentRange, Double]

  def initial(total: NonNegInt): Progress =
    Progress(0.refined, total)

  given Eq[Progress] = Eq.fromUniversalEquals
}
