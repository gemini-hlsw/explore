// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.syntax.all.*
import crystal.Pot
import explore.modes.SpectroscopyModesMatrix
import explore.services.OdbConfigApi
import fs2.Stream
import japgolly.scalajs.react.*
import lucuma.react.common.ReactFnProps

case class ModesCacheController(
  modModes: (Pot[SpectroscopyModesMatrix] => Pot[SpectroscopyModesMatrix]) => IO[Unit]
)(using val odbApi: OdbConfigApi[IO])
    extends ReactFnProps[ModesCacheController](ModesCacheController.component)
    with CacheControllerComponent.Props[SpectroscopyModesMatrix]:
  val modState = modModes

object ModesCacheController
    extends CacheControllerComponent[SpectroscopyModesMatrix, ModesCacheController]:

  override protected val updateStream: ModesCacheController => Resource[
    IO,
    Stream[IO, SpectroscopyModesMatrix => SpectroscopyModesMatrix]
  ] =
    _ => Resource.pure(Stream.empty)

  override protected val initial: ModesCacheController => IO[
    (SpectroscopyModesMatrix, fs2.Stream[IO, SpectroscopyModesMatrix => SpectroscopyModesMatrix])
  ] = props =>
    props.odbApi.spectroscopyModes
      .tupleRight(Stream.empty)
