// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.syntax.all.*
import clue.StreamingClient
import crystal.Pot
import explore.model.SupportedInstruments
import explore.modes.SpectroscopyModeRow
import explore.modes.SpectroscopyModesMatrix
import fs2.Stream
import japgolly.scalajs.react.*
import lucuma.react.common.ReactFnProps
import lucuma.schemas.ObservationDB
import queries.common.ModesQueriesGQL

case class ModesCacheController(
  modModes: (Pot[SpectroscopyModesMatrix] => Pot[SpectroscopyModesMatrix]) => IO[Unit]
)(using client: StreamingClient[IO, ObservationDB])
    extends ReactFnProps[ModesCacheController](ModesCacheController.component)
    with CacheControllerComponent.Props[SpectroscopyModesMatrix]:
  val modState                             = modModes
  given StreamingClient[IO, ObservationDB] = client

object ModesCacheController
    extends CacheControllerComponent[SpectroscopyModesMatrix, ModesCacheController]:

  override protected val updateStream
    : ModesCacheController => Resource[IO, Stream[IO,
                                                  SpectroscopyModesMatrix => SpectroscopyModesMatrix
    ]] =
    _ => Resource.pure(Stream.empty)

  override protected val initial: ModesCacheController => IO[
    (SpectroscopyModesMatrix, fs2.Stream[IO, SpectroscopyModesMatrix => SpectroscopyModesMatrix])
  ] = props =>
    import props.given

    ModesQueriesGQL
      .SpectroscopyModes[IO]
      .query(SupportedInstruments)
      .raiseGraphQLErrors
      .map: u =>
        val modes: List[SpectroscopyModeRow] =
          u.spectroscopyConfigOptions.zipWithIndex.map: (s, i) =>
            s.copy(id = i.some)
        SpectroscopyModesMatrix(modes)
      .tupleRight(Stream.empty)
