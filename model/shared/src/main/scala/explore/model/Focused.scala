// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.Eq
import cats.syntax.all._
import lucuma.core.model.Observation

sealed trait Focused extends Product with Serializable
object Focused {
  case class FocusedObs(obsId: Observation.Id) extends Focused

  implicit val eqFocused: Eq[Focused] = Eq.instance {
    case (FocusedObs(a), FocusedObs(b)) => a === b
    case _                              => false
  }
}
