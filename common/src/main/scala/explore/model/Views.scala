// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.effect._
import crystal._
import explore.graphql.TestQuery

object Views {
  import AppState.rootModel

  lazy val target: View[IO, Option[Target]] = rootModel.view(RootModel.target)
  lazy val persons: View[IO, List[TestQuery.AllPersons]] = rootModel.view(RootModel.persons)
  lazy val todoList: View[IO, List[Task]] = rootModel.view(RootModel.todoList)
}
