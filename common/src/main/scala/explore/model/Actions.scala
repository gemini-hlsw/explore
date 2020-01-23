// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.conditions

import explore.model._
import cats.effect._

object Actions {
  //implicit object TargetActionsIO extends TargetActionsInterpreter[IO](Views.target)
  implicit object PersonsActionsIO extends PersonsActionsInterpreter[IO]//(Views.persons)
}
