// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.implicits._
import cats.effect._
import crystal._
import clue.js.AjaxGraphQLClient
import clue.js.WebSocketGraphQLClient
import explore.graphql.TestQuery
import io.lemonlabs.uri.Url
import monocle.macros.Lenses
import cats.effect.concurrent.Ref
import japgolly.scalajs.react.Reusability

@Lenses
case class RootModel(
  target:   Option[Target]             = None,
  persons:  List[TestQuery.AllPersons] = List.empty,
  todoList: List[Task]                 = List.empty,
  polls:    List[Poll]                 = List.empty
)
object RootModel {
  implicit val filmsReuse: Reusability[TestQuery.AllPersons.Films] = Reusability.derive
  implicit val allPersonsReuse: Reusability[TestQuery.AllPersons]  = Reusability.derive
  implicit val reuse: Reusability[RootModel]                       = Reusability.derive
}

case class AppConfig(
  swapiURL: Url = Url.parse("https://api.graph.cool/simple/v1/swapi"),
  todoURL: Url = Url.parse(
    "https://cors-anywhere.herokuapp.com/https://todo-mongo-graphql-server.herokuapp.com/"
  ),
  pollURL: Url = Url.parse("wss://realtime-poll.demo.hasura.app/v1/graphql")
)

class AppState[F[_]: ConcurrentEffect: Timer](
  config:        AppConfig,
  val rootModel: Model[F, RootModel]
) {
  object Clients {
    val starWars = AjaxGraphQLClient(config.swapiURL)
    val todo     = AjaxGraphQLClient(config.todoURL)
    val polls    = WebSocketGraphQLClient(config.pollURL)
  }

  object Views {
    lazy val target: View[F, Option[Target]]              = rootModel.view(RootModel.target)
    lazy val persons: View[F, List[TestQuery.AllPersons]] = rootModel.view(RootModel.persons)
    lazy val todoList: View[F, List[Task]]                = rootModel.view(RootModel.todoList)
    lazy val polls: View[F, List[Poll]]                   = rootModel.view(RootModel.polls)
  }

  object Actions {
    lazy val persons: PersonsActions[F] = new PersonsActionsInterpreter[F](Clients.starWars)
    lazy val todoList: TodoListActions[F] =
      new TodoListActionsInterpreter[F](Views.todoList)(Clients.todo)
    lazy val polls: PollsActions[F] = new PollsActionsInterpreter[F](Views.polls)(Clients.polls)
  }

  def cleanup(): F[Unit] =
    // Close the websocket
    Clients.polls.close()
}

object AppStateIO {
  import scala.concurrent.ExecutionContext.global

  implicit lazy val timerIO: Timer[IO]     = cats.effect.IO.timer(global)
  implicit lazy val csIO: ContextShift[IO] = IO.contextShift(global)

  private val value: Ref[SyncIO, Option[AppState[IO]]] =
    Ref.unsafe[SyncIO, Option[AppState[IO]]](None)

  def init(config: AppConfig): SyncIO[Unit] =
    value.access.flatMap {
      case (oldValue, set) =>
        val multipleCallsError =
          SyncIO.raiseError[Unit](new Exception("Multiple calls to AppState init."))

        oldValue.fold(
          for {
            model   <- Model.in[SyncIO, IO].of(RootModel(target = Some(Target.M81)))
            success <- set(new AppState[IO](config, model).some)
            _       <- if (success) SyncIO.unit else multipleCallsError
          } yield ()
        )(_ => multipleCallsError)
    }

  def AppState = value.get.unsafeRunSync().getOrElse(throw new Exception("Uninitialized AppState."))
}
