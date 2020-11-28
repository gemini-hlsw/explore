// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import cats.effect._
import cats.syntax.all._
import clue._
import crystal.react.StreamRenderer
import explore.GraphQLSchemas._
import explore.model.reusability._
import explore.utils.ExploreEvent
import io.chrisdavenport.log4cats.Logger
import lucuma.broadcastchannel.BroadcastChannel

case class Clients[F[_]: ConcurrentEffect: Logger](
  exploreDB: GraphQLStreamingClient[F, ExploreDB],
  odb:       GraphQLStreamingClient[F, ObservationDB]
) {
  lazy val ExploreDBConnectionStatus: StreamRenderer.Component[StreamingClientStatus] =
    StreamRenderer.build(exploreDB.statusStream)

  lazy val ODBConnectionStatus: StreamRenderer.Component[StreamingClientStatus] =
    StreamRenderer.build(odb.statusStream)

  def close(): F[Unit] =
    List(exploreDB.close(), odb.close()).sequence.void
}

case class Actions[F[_]](
  // interpreters go here
)

final case class BroadcastChannelCtx[F[_]: Sync](bc: BroadcastChannel[ExploreEvent]) {
  def close(): F[Unit] = Sync[F].delay(bc.close()).attempt.void
}

case class AppContext[F[_]](
  clients:    Clients[F],
  actions:    Actions[F],
  bcc:        BroadcastChannelCtx[F]
)(implicit
  val F:      Applicative[F],
  val cs:     ContextShift[F],
  val timer:  Timer[F],
  val logger: Logger[F]
) {
  val bc: BroadcastChannel[ExploreEvent] = bcc.bc
  def cleanup(): F[Unit]                     =
    clients.close() *> bcc.close()
}

object AppContext {
  def from[F[_]: ConcurrentEffect: ContextShift: Timer: Logger: Backend: StreamingBackend](
    config: AppConfig
  ): F[AppContext[F]] =
    for {
      exploreDBClient <- ApolloStreamingClient.of[F, ExploreDB](config.exploreDBURI)
      odbClient       <- ApolloStreamingClient.of[F, ObservationDB](config.odbURI)
      clients          = Clients(exploreDBClient, odbClient)
      actions          = Actions[F]()
      bc              <-
        Sync[F].delay(new BroadcastChannel[ExploreEvent]("explore")).map(BroadcastChannelCtx(_))

    } yield AppContext[F](clients, actions, bc)
}
