// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.actions

import explore.model._
import cats._
import cats.implicits._
import cats.effect.Async
import explore.graphql._
import clue.GraphQLClient
import diode.data._
import explore.util.Monocle
import crystal.View
import crystal.ActionInterpreter

trait TodoListActions[F[_]]                                        {
  def retrieveAll: F[List[Task]]
  def refresh: F[Unit]
  def toggle(id: String): F[Unit]
}

class TodoListActionInterpreter[F[_]: Async](
  todoClient: GraphQLClient[F]
) extends ActionInterpreter[F, TodoListActions, Pot[List[Task]]] {

  def of(view: View[F, Pot[List[Task]]]) =
    new TodoListActions[F] {
      val retrieveAll: F[List[Task]] = {
        val result = todoClient.query(AllTasksQuery)()
        result.map(_.todos)
      }

      val refresh: F[Unit]           =
        for {
          _     <- view.set(Pending())
          tasks <- retrieveAll
          _     <- view.set(Ready(tasks))
        } yield ()

      def toggle(id: String): F[Unit] =
        todoClient
          .query(ToggleMutation)(ToggleMutation.Variables(id).some)
          .flatMap(_.toggle.map(update).getOrElse(Applicative[F].unit))

      def update(task: Task): F[Unit] = {
        val filterId = Monocle.filteredTraversal[Task](_.id == task.id)
        val replace  = filterId.modify(_ => task)
        view.mod(_.map(replace))
      }
    }
}
