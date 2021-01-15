// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.Eq
import cats.syntax.all._
import lucuma.core.model.{ Observation, Target }
import monocle.macros.Lenses

sealed trait Focused extends Product with Serializable
object Focused {
  @Lenses case class FocusedObs(obsId: Observation.Id) extends Focused
  @Lenses case class FocusedTarget(targetId: Target.Id) extends Focused
  // @Lenses case class FocusedConstraint(constraint: ???) extends Focused
  // @Lenses case class FocusedConfiguration(configuraton: ???) extends Focused

  implicit val eqFocused: Eq[Focused] = Eq.instance {
    case (FocusedObs(a), FocusedObs(b))       => a === b
    case (FocusedTarget(a), FocusedTarget(b)) => a === b
    // case (FocusedConstraint(a), FocusedConstraint(b))       => a === b
    // case (FocusedConfiguration(a), FocusedConfiguration(b)) => a === b
    case _                                    => false
  }
}
