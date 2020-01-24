// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.effect._

object Actions {
  //implicit object TargetActionsIO extends TargetActionsInterpreter[IO](Views.target)
  implicit object PersonsActionsIO extends PersonsActionsInterpreter[IO]//(Views.persons)
  implicit object TodoListActionsIO extends TodoListActionsInterpreter[IO](Views.todoList)
}
