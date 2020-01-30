package explore.model

import crystal._
import cats.effect.ConcurrentEffect
import cats.implicits._
import explore.graphql.polls._
import java.util.UUID

trait PollsActions[F[_]] {
  def retrieveAll(): F[List[Poll]]
  def refresh(): F[Unit]
  def vote(optionId: UUID): F[Unit]
}

class PollsActionsInterpreter[F[_]: ConcurrentEffect](lens: FixedLens[F, List[Poll]])
    extends PollsActions[F] {

  def retrieveAll(): F[List[Poll]] =
    AppState.pollClient.query[F](PollsQuery)().map(_.poll)

  def refresh(): F[Unit] =
    for {
      polls <- retrieveAll()
      _     <- lens.set(polls)
    } yield ()

  def vote(optionId: UUID): F[Unit] =
    AppState.pollClient
      .query[F](VoteMutation)(
        VoteMutation
          .Variables(optionId, UUID.fromString("664ccbe7-b3da-9865-e7cf-8e64ea91897d"))
          .some
      )
      .map(_ => ())
}
