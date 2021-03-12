// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.Eq
import cats.syntax.all._
import lucuma.core.model.Asterism
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.core.model.Target
import monocle.macros.Lenses

sealed trait Focused extends Product with Serializable
object Focused {
  @Lenses case class FocusedObs(obsId: Observation.Id) extends Focused
  @Lenses case class FocusedTarget(targetId: Target.Id) extends Focused
  @Lenses case class FocusedAsterism(asterismId: Asterism.Id) extends Focused
  @Lenses case class FocusedConstraintSet(constraintSetId: ConstraintSet.Id) extends Focused
  // @Lenses case class FocusedConfiguration(configuraton: ???) extends Focused

  implicit val eqFocused: Eq[Focused] = Eq.instance {
    case (FocusedObs(a), FocusedObs(b))                     => a === b
    case (FocusedTarget(a), FocusedTarget(b))               => a === b
    case (FocusedAsterism(a), FocusedAsterism(b))           => a === b
    case (FocusedConstraintSet(a), FocusedConstraintSet(b)) => a === b
    // case (FocusedConfiguration(a), FocusedConfiguration(b)) => a === b
    case _                                                  => false
  }
}
