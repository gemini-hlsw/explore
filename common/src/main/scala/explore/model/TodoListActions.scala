// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import crystal._
import cats._
import cats.implicits._
import cats.effect.Async
import explore.graphql._
import clue.GraphQLClient
import diode.data._
import explore.util.Monocle

trait TodoListActions[F[_]] {
  def retrieveAll: F[List[Task]]
  def refresh: F[Unit]
  def toggle(id: String): F[Unit]
}

class TodoListActionsInterpreter[F[_]: Async](lens: FixedLens[F, Pot[List[Task]]])(
  todoClient:                                       GraphQLClient[F]
) extends TodoListActions[F] {
  val retrieveAll: F[List[Task]] = {
    val result = todoClient.query(AllTasksQuery)()
    result.map(_.todos)
  }

  val refresh: F[Unit] =
    for {
      _     <- lens.set(Pending())
      tasks <- retrieveAll
      _     <- lens.set(Ready(tasks))
    } yield ()

  def toggle(id: String): F[Unit] =
    todoClient
      .query(ToggleMutation)(ToggleMutation.Variables(id).some)
      .flatMap(_.toggle.map(update).getOrElse(Applicative[F].unit))

  def update(task: Task): F[Unit] = {
    val filterId = Monocle.filteredTraversal[Task](_.id == task.id)
    val replace  = filterId.modify(_ => task)
    lens.modify(_.map(replace))
  }
}
