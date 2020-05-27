// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.effect._
import cats.implicits._
import clue._
import crystal.react.StreamRenderer
import explore.model.reusability._
import gem.Observation
import io.chrisdavenport.log4cats.Logger
import japgolly.scalajs.react._
import monocle.macros.Lenses
import org.scalajs.dom
import sttp.model.Uri
import sttp.model.Uri._

@Lenses
case class RootModel(
  obsId:  Option[Observation.Id] = None,
  target: Option[SiderealTarget] = None
)
object RootModel {
  implicit val reuse: Reusability[RootModel] = Reusability.derive
}

case class AppConfig(
  // CORS doesn't kick in for websockets, so we probably don't need proxying for WS.
  programsURL: Uri =
    uri"wss://explore-hasura.herokuapp.com/v1/graphql" //AppConfig.wsBaseUri.path("/api/programs/v1/graphql"),
)
object AppConfig {
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

case class Clients[F[_]: ConcurrentEffect: Logger](
  programs: GraphQLStreamingClient[F]
) {
  protected implicit val statusReuse: Reusability[StreamingClientStatus] = Reusability.derive

  lazy val programsConnectionStatus =
    StreamRenderer.build(programs.statusStream)

  def close(): F[Unit] =
    programs.close()
}

case class Actions[F[_]](
  // interpreters go here
)

case class AppContext[F[_]](
  clients:    Clients[F],
  actions:    Actions[F]
)(implicit
  val cs:     ContextShift[F],
  val timer:  Timer[F],
  val logger: Logger[F]
) {
  def cleanup(): F[Unit] =
    clients.close()
}

object AppContext {
  def from[F[_]: ConcurrentEffect: ContextShift: Timer: Logger: Backend: StreamingBackend](
    config: AppConfig
  ): F[AppContext[F]] =
    for {
      programsClient <- ApolloStreamingClient.of(config.programsURL)
      clients         = Clients(programsClient)
      actions         = Actions[F]()
    } yield AppContext[F](clients, actions)
}
