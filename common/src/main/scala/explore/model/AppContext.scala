// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.effect._
import cats.syntax.all._
import clue._
import crystal.react.StreamRenderer
import explore.GraphQLSchemas._
import explore.model.reusability._
import io.chrisdavenport.log4cats.Logger
import sttp.model.Uri

case class AppConfig(odbURI: Uri)

case class Clients[F[_]: ConcurrentEffect: Logger](
  odb: GraphQLStreamingClient[F, ObservationDB]
) {
  lazy val ODBConnectionStatus =
    StreamRenderer.build(odb.statusStream)

  def close(): F[Unit] =
    odb.close()
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
      programsClient <- ApolloStreamingClient.of[F, ObservationDB](config.odbURI)
      clients         = Clients(programsClient)
      actions         = Actions[F]()
    } yield AppContext[F](clients, actions)
}
