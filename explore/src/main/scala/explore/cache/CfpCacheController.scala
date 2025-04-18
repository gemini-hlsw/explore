// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.syntax.all.*
import clue.StreamingClient
import crystal.Pot
import explore.model.CallForProposal
import fs2.Stream
import japgolly.scalajs.react.*
import lucuma.react.common.ReactFnProps
import lucuma.schemas.ObservationDB
import queries.common.CallsQueriesGQL.ReadOpenCFPs

case class CfpCacheController(
  modCalls: (Pot[List[CallForProposal]] => Pot[List[CallForProposal]]) => IO[Unit]
)(using client: StreamingClient[IO, ObservationDB])
    extends ReactFnProps[CfpCacheController](CfpCacheController.component)
    with CacheControllerComponent.Props[List[CallForProposal]]:
  val modState                             = modCalls
  given StreamingClient[IO, ObservationDB] = client

object CfpCacheController
    extends CacheControllerComponent[List[CallForProposal], CfpCacheController]:

  override protected val updateStream
    : CfpCacheController => Resource[IO,
                                     Stream[IO, List[CallForProposal] => List[CallForProposal]]
    ] =
    _ => Resource.pure(Stream.empty)

  override protected val initial: CfpCacheController => IO[
    (List[CallForProposal], fs2.Stream[IO, List[CallForProposal] => List[CallForProposal]])
  ] = props =>
    import props.given

    ReadOpenCFPs[IO]
      .query()
      .raiseGraphQLErrors
      .map(_.callsForProposals.matches)
      .tupleRight(Stream.empty)
