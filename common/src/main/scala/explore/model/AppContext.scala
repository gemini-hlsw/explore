package explore.model

import cats.effect._
import cats.syntax.all._
import clue._
import io.chrisdavenport.log4cats.Logger
import sttp.model.Uri

case class AppConfig(programsURL: Uri)

case class Clients[F[_]: ConcurrentEffect: Logger](
  programs: GraphQLStreamingClient[F]
) {
  // lazy val programsConnectionStatus =
  // StreamRenderer.build(programs.statusStream)

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
      programsClient <- ApolloStreamingClient.of[F](config.programsURL)
      clients         = Clients(programsClient)
      actions         = Actions[F]()
    } yield AppContext[F](clients, actions)
}
