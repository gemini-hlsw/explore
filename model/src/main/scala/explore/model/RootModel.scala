// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.effect._
import cats.implicits._
import cats.kernel.Eq
import clue._
import explore.model.enum.AppTab
import gem.Observation
import gem.data.EnumZipper
import io.chrisdavenport.log4cats.Logger
import monocle.Optional
import monocle.Prism
import monocle.function.Possible.possible
import monocle.macros.GenPrism
import monocle.macros.Lenses
import sttp.model.Uri
import sttp.model.Uri._
import monocle.Lens

@Lenses
case class RootModel(
  tabs:    EnumZipper[AppTab],
  focused: Option[Focused] = None
)

object RootModel {
  implicit val eqRootModel: Eq[RootModel] = Eq.by(m => (m.tabs, m.focused))

  val focusedOpt: Optional[RootModel, Focused]                                  =
    focused.composeOptional(possible)
  val focusedObsId: Optional[RootModel, ExploreObservation.Id]                  =
    focusedOpt.composeOptional(Focused.obsId)
  val focusedTargetId: Optional[RootModel, SiderealTarget.Id]                   =
    focusedOpt.composeOptional(Focused.targetId)
  val focusedTargetOrObsId
    : Lens[RootModel, Option[Either[SiderealTarget.Id, ExploreObservation.Id]]] =
    Lens[RootModel, Option[Either[SiderealTarget.Id, ExploreObservation.Id]]](
      _.focused.flatMap(Focused.targetOrObsId.getOption)
    )(opt => _.copy(focused = opt.map(Focused.targetOrObsId.reverseGet)))
}

case class AppConfig(
  // CORS doesn't kick in for websockets, so we probably don't need proxying for WS.
  programsURL: Uri =
    uri"wss://explore-db.herokuapp.com/v1/graphql" //AppConfig.wsBaseUri.path("/api/programs/v1/graphql"),
)
object AppConfig {
  // lazy val baseUri: Uri = {
  //   val location = dom.window.location.toString
  //   Uri.parse(location).getOrElse(throw new Exception(s"Could not parse URL [$location]"))
  // }

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
      programsClient <- ApolloStreamingClient.of(config.programsURL)
      clients         = Clients(programsClient)
      actions         = Actions[F]()
    } yield AppContext[F](clients, actions)
}
