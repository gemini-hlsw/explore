// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.syntax.all.*
import clue.StreamingClient
import explore.DefaultErrorPolicy
import explore.model.CallForProposal
import fs2.Stream
import japgolly.scalajs.react.*
import lucuma.react.common.ReactFnProps
import lucuma.schemas.ObservationDB
import queries.common.CallsQueriesGQL.ReadOpenCFPs

case class CfpCache(
  setCalls: Option[List[CallForProposal]] => IO[Unit]
)(using client: StreamingClient[IO, ObservationDB])
    extends ReactFnProps[CfpCache](CfpCache.component)
    with CacheComponent.Props[List[CallForProposal]]:
  val setState                             = setCalls
  given StreamingClient[IO, ObservationDB] = client

object CfpCache extends CacheComponent[List[CallForProposal], CfpCache]:

  override protected val updateStream
    : CfpCache => Resource[IO, Stream[IO, List[CallForProposal] => List[CallForProposal]]] =
    _ => Resource.pure(Stream.empty)

  given Reusability[CfpCache] = Reusability.always

  override protected val initial: CfpCache => IO[
    (List[CallForProposal], fs2.Stream[IO, List[CallForProposal] => List[CallForProposal]])
  ] = props =>
    import props.given

    ReadOpenCFPs[IO]
      .query()
      .map(_.map(_.callsForProposals.matches))
      .tupleRight(Stream.empty)
