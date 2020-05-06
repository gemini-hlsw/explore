// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.implicits._
import cats.effect._
import clue._
import sttp.model.Uri
import sttp.model.Uri._
import monocle.macros.Lenses
import japgolly.scalajs.react._
import io.chrisdavenport.log4cats.Logger
import org.scalajs.dom
import crystal.react.StreamRenderer
import gem.Observation

@Lenses
case class RootModel(
  id:     Option[Observation.Id] = None,
  target: Option[ExploreSiderealTarget] = None
)
object RootModel  {
  import explore.model.reusability._
  implicit val observationIdReuse: Reusability[Observation.Id] =
    Reusability.never // This is just for temporary testing!!!!
  implicit val reuse: Reusability[RootModel] = Reusability.derive
}

case class AppConfig(
  // CORS doesn't kick in for websockets, so we probably don't need proxying for WS.
  conditionsURL: Uri =
    uri"wss://explore-hasura.herokuapp.com/v1/graphql", //AppConfig.wsBaseUri.path("/api/conditions/v1/graphql"),
  swapiURL:      Uri =
    AppConfig.baseUri.path("/api/grackle-demo/starwars"), //"https://api.graph.cool/simple/v1/swapi"
  todoURL:       Uri = AppConfig.baseUri.path("/api/tasks"),
  pollURL:       Uri = uri"wss://realtime-poll.demo.hasura.app/v1/graphql"
)
object AppConfig  {
  lazy val baseUri: Uri = {
    val location = dom.window.location.toString
    Uri.parse(location).getOrElse(throw new Exception(s"Could not parse URL [$location]"))
  }

  /*lazy val wsBaseUri: Uri = {
    val uri = baseUri
    val scheme = uri.scheme match {
      case "https" => "wss"
      case _       => "ws"
    }
    uri.scheme(scheme)
  }*/
}

case class Clients[F[_]: ConcurrentEffect](
  conditions: GraphQLStreamingClient[F],
  starWars:   GraphQLClient[F],
  todo:       GraphQLClient[F],
  polls:      GraphQLStreamingClient[F]
)                 {
  lazy val pollConnectionStatus =
    StreamRenderer.build(polls.statusStream, Reusability.derive)

  def close(): F[Unit] =
    polls.close()
}

case class Actions[F[_]](
  // interpreters go here
)

case class AppContext[F[_]](
  clients:   Clients[F],
  actions:   Actions[F]
)(implicit
  val cs:    ContextShift[F],
  val timer: Timer[F]
)                 {
  def cleanup(): F[Unit] =
    clients.close()
}

object AppContext {
  def from[F[_]: ConcurrentEffect: ContextShift: Timer: Logger: Backend: StreamingBackend](
    config: AppConfig
  ): F[AppContext[F]] =
    for {
      conditionsClient <- ApolloStreamingClient.of(config.conditionsURL)
      swClient         <- HttpClient.of(config.swapiURL)
      todoClient       <- HttpClient.of(config.todoURL)
      pollsClient      <- ApolloStreamingClient.of(config.pollURL)
      clients           = Clients(
                  conditionsClient,
                  swClient,
                  todoClient,
                  pollsClient
                )
      actions           = Actions[F]()
    } yield AppContext[F](clients, actions)
}
