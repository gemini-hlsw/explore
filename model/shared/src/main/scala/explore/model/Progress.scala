// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import coulomb._
import coulomb.syntax._
import coulomb.units.accepted._
import eu.timepit.refined.api._
import eu.timepit.refined.numeric._
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import lucuma.refined._

case class Progress private (current: NonNegInt, total: NonNegInt) {
  lazy val percentage: Quantity[Progress.PercentRange, Percent] =
    Progress.PercentRange
      .unsafeFrom(current.value * 100.0 / total.value)
      .withUnit[Percent]

  def increment(steps: PosInt = 1.refined): Progress =
    if (current.value + steps.value > total.value)
      this
    else
      Progress(NonNegInt.unsafeFrom(current.value + steps.value), total)
}

object Progress {
  type PercentRange = Double Refined Interval.Closed[0.0, 100.0]
  object PercentRange extends RefinedTypeOps.Numeric[PercentRange, Double]

  def initial(total: NonNegInt): Progress =
    Progress(0.refined, total)

  implicit val eqProgress: Eq[Progress] = Eq.fromUniversalEquals
}
