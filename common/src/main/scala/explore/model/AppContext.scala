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

case class Clients[F[_]: ConcurrentEffect: Logger](
  exploreDB: GraphQLPersistentStreamingClient[F, ExploreDB],
  odb:       GraphQLPersistentStreamingClient[F, ObservationDB]
) {
  lazy val ExploreDBConnectionStatus: StreamRenderer.Component[StreamingClientStatus] =
    StreamRenderer.build(exploreDB.statusStream)

  lazy val ODBConnectionStatus: StreamRenderer.Component[StreamingClientStatus] =
    StreamRenderer.build(odb.statusStream)

  def disconnect(): F[Unit] =
    List(exploreDB.disconnect(), odb.disconnect()).sequence.void
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
    clients.disconnect()
}

object AppContext {
  def from[F[_]: ConcurrentEffect: ContextShift: Timer: Logger: Backend: PersistentBackend](
    config: AppConfig
  ): F[AppContext[F]] =
    for {
      exploreDBClient <- ApolloStreamingClient.of[F, ExploreDB](config.exploreDBURI)
      odbClient       <- ApolloStreamingClient.of[F, ObservationDB](config.odbURI)
      clients          = Clients(exploreDBClient, odbClient)
      actions          = Actions[F]()
    } yield AppContext[F](clients, actions)
}
