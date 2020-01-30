package explore.model

import crystal._
import cats.effect.Async
import cats.implicits._
import explore.graphql._

trait TodoListActions[F[_]] {
  def retrieveAll(): F[List[Task]]
  def refresh(): F[Unit]
  def toggle(id: String): F[Unit]
}

class TodoListActionsInterpreter[F[_]: Async](lens: FixedLens[F, List[Task]])
    extends TodoListActions[F] {
  def retrieveAll(): F[List[Task]] = {
    val result = AppState.todoClient.query[F](AllTasksQuery)()
    result.map(_.todos)
  }

  def refresh(): F[Unit] =
    for {
      tasks <- retrieveAll()
      _     <- lens.set(tasks)
    } yield ()

  def toggle(id: String): F[Unit] =
    AppState.todoClient
      .query[F](ToggleMutation)(ToggleMutation.Variables(id).some)
      .map(_ => ())

}
