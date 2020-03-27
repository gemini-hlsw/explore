// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.implicits._
import cats.effect._
import crystal._
import clue._
import clue.js._
import io.lemonlabs.uri.Url
import monocle.macros.Lenses
import japgolly.scalajs.react._
import diode.data._
import explore.util.Pot._
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.log4s.Log4sLogger
import clue.Backend
import clue.HttpClient

@Lenses
case class RootModel(
  target:   Option[Target]             = None,
  todoList: Pot[List[Task]]            = Pot.empty,
  polls:    Pot[List[Poll]]            = Pot.empty
)
object RootModel {
  implicit val reuse: Reusability[RootModel]                       = Reusability.derive
}

case class AppConfig(
  swapiURL: Url = Url.parse("/api/grackle-demo/starwars"), //"https://api.graph.cool/simple/v1/swapi"
  todoURL:  Url = Url.parse("/api/tasks"),
  pollURL:  Url = Url.parse("wss://realtime-poll.demo.hasura.app/v1/graphql")
)

case class Clients[F[_]](
  starWars: GraphQLClient[F],
  todo:     GraphQLClient[F],
  polls:    GraphQLStreamingClient[F]
) {
  def close(): F[Unit] =
    polls.close()
}

case class Views[F[_]](
  target:   View[F, Option[Target]],
  todoList: View[F, Pot[List[Task]]],
  polls:    View[F, Pot[List[Poll]]]
)

case class Actions[F[_]](
  todoList: TodoListActions[F],
  polls:    PollsActions[F]
)

case class ApplicationState[F[_]](
  rootModel: Model[F, RootModel],
  clients:   Clients[F],
  views:     Views[F],
  actions:   Actions[F]
)(
  implicit
  val cs:    ContextShift[F],
  val timer: Timer[F]
) {
  def cleanup(): F[Unit] =
    clients.close()
}

object ApplicationState {
  def from[F[_]: ConcurrentEffect: ContextShift: Timer: Logger: Backend: StreamingBackend](
    config: AppConfig
  ): F[ApplicationState[F]] =
    for {
      model       <- Model[F].of(RootModel(target = Some(Target.M81)))
      swClient    <- HttpClient.of(config.swapiURL)
      todoClient  <- HttpClient.of(config.todoURL)
      pollsClient <- ApolloStreamingClient.of(config.pollURL)
      clients = Clients(
        swClient,
        todoClient,
        pollsClient
      )
      views = Views(
        model.view(RootModel.target),
        model.view(RootModel.todoList),
        model.view(RootModel.polls)
      )
      actions = Actions(
        new TodoListActionsInterpreter[F](views.todoList)(clients.todo),
        new PollsActionsInterpreter[F](views.polls)(pollsClient)
      )
    } yield {
      ApplicationState[F](model, clients, views, actions)
    }
}

object AppStateIO {
  private var value: ApplicationState[IO] = null

  def AppState: ApplicationState[IO] =
    Option(value).getOrElse(throw new Exception("Uninitialized AppState!"))

  implicit def csIO: ContextShift[IO] = AppState.cs

  implicit def timerIO: Timer[IO] = AppState.timer

  def init(
    config:           AppConfig
  )(implicit timerIO: Timer[IO], csIO: ContextShift[IO]): IO[ApplicationState[IO]] =
    Option(value).fold {
      implicit val logger
        : Logger[IO] = Log4sLogger.createLocal[IO] // Must be here since it needs ContextShift[IO]

      implicit val gqlHttpBackend: Backend[IO] = AjaxJSBackend[IO]

      implicit val gqlStreamingBackend: StreamingBackend[IO] = WebSocketJSBackend[IO]

      ApplicationState.from[IO](config).flatTap(appState => IO { value = appState })
    }(_ => IO.raiseError(new Exception("Multiple calls to AppState init.")))
}
