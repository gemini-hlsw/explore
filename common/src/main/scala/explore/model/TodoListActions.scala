package explore.model

import crystal._
import cats.effect.Async
import cats.implicits._
import explore.graphql._
import clue.GraphQLClient

trait TodoListActions[F[_]] {
  def retrieveAll(): F[List[Task]]
  def refresh(): F[Unit]
  def toggle(id: String): F[Unit]
}

class TodoListActionsInterpreter[F[_]: Async](lens: FixedLens[F, List[Task]])(todoClient: GraphQLClient[F])
    extends TodoListActions[F] {
  def retrieveAll(): F[List[Task]] = {
    val result = todoClient.query(AllTasksQuery)()
    result.map(_.todos)
  }

  def refresh(): F[Unit] =
    for {
      tasks <- retrieveAll()
      _     <- lens.set(tasks)
    } yield ()

  def toggle(id: String): F[Unit] =
    todoClient
      .query(ToggleMutation)(ToggleMutation.Variables(id).some)
      .map(_ => ())

}
