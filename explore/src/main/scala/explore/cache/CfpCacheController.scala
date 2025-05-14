// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.syntax.all.*
import crystal.Pot
import explore.model.CallForProposal
import explore.services.OdbProposalApi
import fs2.Stream
import japgolly.scalajs.react.*
import lucuma.react.common.ReactFnProps

case class CfpCacheController(
  modCalls: (Pot[List[CallForProposal]] => Pot[List[CallForProposal]]) => IO[Unit]
)(using val odbApi: OdbProposalApi[IO])
    extends ReactFnProps[CfpCacheController](CfpCacheController.component)
    with CacheControllerComponent.Props[List[CallForProposal]]:
  val modState         = modCalls
  val onLoad: IO[Unit] = IO.unit

object CfpCacheController
    extends CacheControllerComponent[List[CallForProposal], CfpCacheController]:

  override protected val updateStream: CfpCacheController => Resource[
    IO,
    Stream[IO, List[CallForProposal] => List[CallForProposal]]
  ] =
    _ => Resource.pure(Stream.empty)

  override protected val initial: CfpCacheController => IO[
    (List[CallForProposal], fs2.Stream[IO, List[CallForProposal] => List[CallForProposal]])
  ] = props =>
    props.odbApi
      .openCfps()
      .tupleRight(Stream.empty)
